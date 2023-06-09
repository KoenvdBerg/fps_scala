import scala.Conversion
import scala.language.implicitConversions
import testing.*

import scala.util.matching.Regex

object chapter09:
  trait Parsers[Parser[+_]]:
    private val self: Parsers[Parser] = this
    case class ParserError(stack: List[(Location, String)])

    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(using f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    // PARSER CASE CLASS WITH COMBINATORS
    case class ParserOps[A](p: Parser[A]):
      def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def map[AA>:A, B](f: A => B): Parser[B] = self.map(p)(f)
      def flatMap[AA>:A, B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
      def many[AA>:A]: Parser[List[A]] = self.many(p)
      def many1[AA>:A]: Parser[List[A]] = self.many1(p)
      def slice[A]: Parser[String] = self.slice(p)
      def **[B>:A](p2: Parser[B]): Parser[(A, B)] = self.product(p,p2)

    // PRIMITIVES
    implicit def string(s: String): Parser[String] = ???
    implicit def regex(s: Regex): Parser[String] = ???
    def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???
    def run[A](p: Parser[A])(input: String): Either[ParserError, A] = ???
    def succeed[A](a: A): Parser[A] = string("").map(_ => a)
    def slice[A](p: Parser[A]): Parser[String] = ???
    def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
      // without FOR: flatMap(p)(pp => p2.map(pp2 => (pp, pp2)))
      for {
        pp1 <- p
        pp2 <- p2
      } yield (pp1, pp2)
    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      for {
        pp1 <- p
        pp2 <- p2
      } yield f(pp1, pp2)
      // OLD: product(p, p2).map((a: (A, B)) => f(a._1, a._2))
      // without FOR: flatMap(p)(pp => p2.map(pp2 => f(pp, pp2)))
    def map[A, B](p2: Parser[A])(f: A => B): Parser[B] =
        map2(p2, succeed(()))((a, _) => f(a))
        // option 2: flatMap(p2)(pp => succeed(f(pp)))
    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

    // ERROR REPORTING
    def label[A](msg: String)(p: Parser[A]): Parser[A] = ???
    def errorLocation(e: ParserError): Location = ???
    def errorMessage(e: ParserError): String = ???
    def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???
    def attempt[A](p: Parser[A]): Parser[A] = ???


    // HELPER FUNCTIONS
    def many[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _) | succeed(List())
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if n <= 0 then succeed(Nil)
      else
        map2(p, listOfN(n - 1, p))(_ :: _)
    def count[A](p: Parser[List[A]]): Parser[Int] = p.map((as: List[A]) => as.length)
    def char(c: Char): Parser[Char] = string(c.toString).map((s: String) => s.charAt(0))
    def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
    def other[A, B](p: Parser[A], p2: Parser[B]): Parser[B] =
      for {_ <- p; pp <- p2} yield pp
    def sequence[A](sep: Parser[Char], p: Parser[A]): Parser[List[A]] =
      for {
        ppl <- p
        next <- other(sep, p).many
      } yield ppl :: next
    def skipWhiteSpace[A](p: Parser[A]): Parser[A] =
      for {
        _ <- whitespace.many.slice
        pp <- p
        _ <- whitespace.many.slice
      } yield pp

    // PARSER EXAMPLES
    def zeroOrMore[A](p: Parser[A]): Parser[Int] = count(p.many)
    val numA: Parser[Int] = char('a').many.slice.map((f: String) => f.length)
    val numAB: Parser[(Int, Int)] = char('a').many.slice.map((s: String) => s.length)
      ** char('b').many1.slice.map((s: String) => s.length)
    val contextSensitive_96: Parser[Int] = digit.flatMap(dd => listOfN(dd, char('a')).map(_.size))
    val contextSensitiveFor: Parser[Int] = for {
      dd <- digit
      n <- listOfN(dd, char('a'))
    } yield n.length


    // COMBINATORS
    val digit: Parser[Int] = skipWhiteSpace(regex("""[0-9]""".r).map(_.toInt))
    val letter: Parser[String] = skipWhiteSpace(regex("""[a-Z\s]""".r))
    val whitespace: Parser[String] = regex("""[\s\t\r\n\f]""".r)
    val number: Parser[String] =  digit.slice
    val decimal: Parser[String] = for {
      n1 <- number
      dot <- char('.')
      n2 <- number
    } yield n1 + dot + n2
    val scientific: Parser[String] = for {
      d <- decimal
      e <- char('e') | char('E')
      n <- number
    } yield d + e + n
    val bool: Parser[Boolean] = (string("true") | string("false")).map(_.toBoolean)

    // PROPERTY LAWS FOR THIS ALGEBRA
    object laws:
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
      def succeedLaw2[A](in: Gen[String]): Prop =
        Prop.forAll(in)(
          (f: String) =>
            run(succeed("koen"))(f) == Right("koen")
        )

      def productLaw(p1: Parser[Int], p2: Parser[Int])(in: Gen[String]): Prop =
          val c1: Parser[(Int, Int)] = succeed((1, 1))
          val c2: Parser[(Int, Int)] = product(p1, p2)
          equal(c1, c2)(in)


      def productLaw2[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
        val c1: Parser[(A, B)] = p1.flatMap((pp1: A) => p2.map((pp2: B) => (pp1, pp2)))
        val c2: Parser[(A, B)] = product(p1, p2)
        equal(c1, c2)(in)

      def productProp: Prop =
        Prop.check{
          val c1: Parser[String] = succeed("a")
          run(product(c1, c1))("a") == Right(("a", "a"))
        }

      def labelLaw[A](p: Parser[A], inputs: Gen[String]): Prop =
        Prop.forAll(inputs.map(i => (i, "sample error message"))){
          case (input: String, msg: String) =>
            run(label(msg)(p))(input) match
              case Left(e) => errorMessage(e) == msg
              case _ => true
        }



  object Parsers:
    val  lel = 8

  case class Location(input: String, offset: Int = 0):
    lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match
      case -1 => offset + 1
      case lineStart => offset - lineStart



@main def run_chapter09: Unit =
  ()

  // TODO: continue from page 160 (9.5)


