import scala.util.matching.Regex
import chapter09.Parsers

import java.awt.Panel
import scala.language.implicitConversions

val example: String =
  """
  {
    "company name": "Microsoft Corporation",
    "ticker" : "MSFT",
    "active" : true,
    "price"  : 30.66,
    "shares outstanding" : 8.38e9,
    "related companies"  : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
  }
  """

object Combinator:

  case class Location(input: String, offset: Int = 0):
    lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match
      case -1 => offset + 1
      case lineStart => offset - lineStart
    def errorLocation(e: ParserError): Location = ???
    def errorMessage(e: ParserError): String = ???

  case class ParserError(stack: List[(Location, String)])


  // type Parser[+A] = State[String, Either[String, A]]
  type Parser[+A] = String => (Either[String, A], String)
  // type Parser[+A] = String => Either[String, A]

  implicit def operators[A](p: Parser[A]): P[A] = P[A](p)
  case class P[+A](p: Parser[A]):
    def map[AA>:A, B](f: A => B): Parser[B] =
      P.flatMap(p)(pp => P.succeed(f(pp)))
    def flatMap[AA>:A, B](f: A => Parser[B]): Parser[B] = P.flatMap(p)(f)
    def |[B >: A](p2: Parser[B]): Parser[B] = P.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = P.or(p, p2)
    def many[AA >: A]: Parser[List[A]] = P.many(p)
    def many1[AA >: A]: Parser[List[A]] = P.many1(p)
    def slice[A]: Parser[String] = P.slice(p)
    def **[B >: A](p2: Parser[B]): Parser[(A, B)] = P.product(p, p2)

  object P:
    // PRIMITIVES
    def run[A](p: Parser[A])(input: String): Either[String, A] = p(input)._1
    implicit def string(s: String): Parser[String] =
      (i: String) =>
        val (h, next): (String, String) = i.splitAt(s.length)
        // if s.isEmpty then Right(s)
        if h == s then (Right(h), next)
        else (Left("ERROR reporting to be included"), next)
    implicit def regex(s: Regex): Parser[String] =
      (i: String) =>
        s.findPrefixOf(i) match
          case Some(s) =>
            val next: String = i.replaceFirst(s, "")
            (Right(s), next)
          case None    => (Left("ERROR reporting to be included"), i)
    def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] =
      (i: String) =>
        s1(i) match
          case (r@Right(_), s) => (r, s)
          // TODO: implement error handling below:
          case (Left(_), _)    => s2(i)
    def succeed[A](a: A): Parser[A] =
      // map(string(""))(_ => a)
      (i: String) => (Right(a), i)

    def slice[A](p: Parser[A]): Parser[String] = ???
    def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
     flatMap(p)(pp => map(p2)(pp2 => (pp, pp2)))
    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      flatMap(p)(pp => map(p2)(pp2 => f(pp, pp2)))
    def map[A, B](p2: Parser[A])(f: A => B): Parser[B] =
      flatMap(p2)(pp => succeed(f(pp)))
    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
      (i: String) =>
        p(i) match
          case (Right(v), next) => f(v)(next)
          //TODO: implement error handling below:
          case (Left(e), next)  => (Left(e), next)

    // HELPER FUNCTIONS
    def many[A](p: Parser[A]): Parser[List[A]] =
      or(map2(p, many(p))(_ :: _), succeed(List()))

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if n <= 0 then succeed(Nil)
      else
        map2(p, listOfN(n - 1, p))(_ :: _)

    def count[A](p: Parser[List[A]]): Parser[Int] = map(p)((as: List[A]) => as.length)

    def char(c: Char): Parser[Char] = map(string(c.toString))((s: String) => s.charAt(0))

    def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

    def other[A, B](p: Parser[A], p2: Parser[B]): Parser[B] =
      for {_ <- p; pp <- p2} yield pp

//    def sequence[A](sep: Parser[Char], p: Parser[A]): Parser[List[A]] =
//      for {
//        ppl <- p
//        next <- other(sep, p).many
//      } yield ppl :: next


@main def testP: Unit =
  import State.*
  import testing.*
  import Combinator.*
  import Combinator.P.*

  val stringGen: Gen[String] = Gen.stringOfN(10, Gen.char)
  val parserTest: Parser[String] = P.string("Koen")

  def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
    Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

  def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
    equal(p, p.map(a => a))(in)

  def succeedLaw1: Prop =
    Prop.check {
      val p1 = succeed("lkd")
      val p2 = Right("lkd")
      run(p1)("skdfj") == p2
    }

  def succeedLaw2(in: Gen[String]): Prop =
    Prop.forAll(in)(
      (f: String) =>
        run(succeed("koen"))(f) == Right("koen")
    )

  def productLaw(in: Gen[String]): Prop =
    val c1: Parser[(Int, Int)] = succeed((1, 1))
    val c2: Parser[(Int, Int)] = product(succeed(1), succeed(1))
    equal(c1, c2)(in)


  def productLaw2[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
    val c1: Parser[(A, B)] = p1.flatMap((pp1: A) => p2.map((pp2: B) => (pp1, pp2)))
    val c2: Parser[(A, B)] = product(p1, p2)
    equal(c1, c2)(in)

  def productProp: Prop =
    Prop.check {
      val c1: Parser[String] = succeed("a")
      run(product(c1, c1))("a") == Right(("a", "a"))
    }

  def listOfNCheck: Prop =
    Prop.check {
      val x: Parser[List[String]] = listOfN(3, string("ab") | string("cad"))
      run(x)("ababcad") == Right(List("ab", "ab", "cad"))
    }

  def manyCheck(in: Gen[String]): Prop =
    val x: Parser[Int] = many(regex("""[a-zA-Z]""".r)).map(_.length)
    val y: Parser[Int] = (i: String) => (Right(i.length), i)
    equal(x, y)(in)

  def regexCheck: Prop =
    Prop.check {
      val x: Parser[List[String]] = listOfN(2, regex("aged:[0-9]".r))
      val result1: Either[String, List[String]] = run(x)("aged:9aged:5")
      val result2: Either[String, List[String]] = run(x)("aged:9aged:five")
      result1 == Right(List("aged:9", "aged:5")) &&
      result2 == Left("ERROR reporting to be included")
    }



  // RUNNING TESTS:
  Prop.run(mapLaw(parserTest)(stringGen))
  Prop.run(succeedLaw1)
  Prop.run(succeedLaw2(stringGen))
  Prop.run(productLaw(stringGen))
  Prop.run(productLaw2(P.succeed(100), P.char('s'))(stringGen))
  Prop.run(productProp)
  Prop.run(listOfNCheck)
  Prop.run(manyCheck(stringGen))
  Prop.run(regexCheck)

//  def labelLaw[A](p: Parser[A], inputs: Gen[String]): Prop =
//    Prop.forAll(inputs.map(i => (i, "sample error message"))) {
//      case (input: String, msg: String) =>
//        run(label(msg)(p))(input) match
//          case Left(e) => errorMessage(e) == msg
//          case _ => true
//    }





