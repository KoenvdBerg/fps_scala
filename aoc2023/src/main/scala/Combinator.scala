package aoc2022

import scala.util.matching.Regex
import scala.language.implicitConversions

object Combinator:

  case class Location(input: String, index: Int = 0, errorIndex: Int = 0, trace: String = "baz"):
    lazy val line: Int = input.slice(0, errorIndex + 1).count(_ == '\n') + 1
    lazy val prevLine: Int = input.slice(0, errorIndex + 1).lastIndexOf('\n')
    lazy val nextLine: Int = input.slice(errorIndex, errorIndex + 1000).indexWhere(_ == '\n')
    lazy val col: Int = prevLine match
      case -1        => errorIndex + 1
      case lineStart => errorIndex - lineStart
    lazy val thisLine: String = nextLine match
      case -1        => input.slice(prevLine, input.length)
      case endOfLine => input.slice(prevLine, errorIndex + endOfLine)

    def updateErrorIndex(i: Int, t: String): Location = Location(input, index, i, t)

    def makeErrorMessage(exp: String): String =
      val errors: String = if index == errorIndex then
        errorMessage(exp)
      else
        errorMessage(trace) + Location(input, index, index, trace).errorMessage(exp)
      s"\n--------  PARSING ERROR(S)  --------" + errors


    def errorMessage(exp: String): String =
      s"""
         |Location: (line: $line, col: $col, total chars parsed: $errorIndex)
         |Expected: '$exp'
         |$thisLine
         |${List.fill(col-1)(" ").mkString("") + "^"}
         |${List.fill((col-5).max(0))(" ").mkString("") + "ERROR HERE"}
         |""".stripMargin


  type Parser[+A] = Location => (Either[String, A], Location)

  object Parser:
    // CONSTRUCTORS
    def succeed[A](a: A): Parser[A] =
      (i: Location) => (Right(a), i)

    def string(s: String): Parser[String] =
      (i: Location) =>
        val searchUntil: Int = s.length + i.index
        val h: String = i.input.slice(i.index, searchUntil)
        if h == s then
          (Right(h), Location(i.input, searchUntil, searchUntil.max(i.errorIndex),
            if searchUntil > i.errorIndex then s else i.trace))
        else
          (Left(i.makeErrorMessage(s)), i)
    def regex(s: Regex): Parser[String] =
      (i: Location) =>
        val (_, searchSpace): (String, String) = i.input.splitAt(i.index)
        s.findPrefixOf(searchSpace) match
          case Some(s) =>
            val searchUntil: Int = i.index + s.length
            (Right(s), Location(i.input, searchUntil, searchUntil.max(i.errorIndex),
              if searchUntil > i.errorIndex then s else i.trace))
          case None    =>
            (Left(i.makeErrorMessage(s.toString())), i)

    def char(c: Char): Parser[Char] = map(string(c.toString))((s: String) => s.charAt(0))

    // EXTENSIONS
    extension[A] (self: Parser[A])
      def flatMap[B](f: A => Parser[B]): Parser[B] =
        (i: Location) =>
          self(i) match
            case (Right(v), loc) => f(v)(loc)
            case (Left(e), loc) => (Left(e), i.updateErrorIndex(loc.errorIndex, loc.trace))

      def map[B](f: A => B): Parser[B] =
        self.flatMap(pp => succeed(f(pp)))

      def or(s2: => Parser[A]): Parser[A] =
        (i: Location) =>
          self(i) match
            case (Left(_), loc) => s2(i.updateErrorIndex(loc.errorIndex, loc.trace))
            case r              => r

      def |(s2: => Parser[A]): Parser[A] = self.or(s2)

      def run(input: String): Either[String, A] = self(Location(input))._1

      def opt: Parser[Option[A]] =
        (i: Location) =>
          self(i) match
            case (Right(v), loc) => (Right(Some(v)), loc)
            case (Left(_), loc) => (Right(None), loc)

      def slice: Parser[String] =
        self.map {
          case Nil => ""
          case h :: t => (h :: t).mkString("")
          case x => x.toString
        }

      def product[B](p2: => Parser[B]): Parser[(A, B)] =
        self.flatMap(pp => p2.map(pp2 => (pp, pp2)))

      def **[B](p2: => Parser[B]): Parser[(A, B)] =
        self.product(p2)

      def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
        self.flatMap(pp => p2.map(pp2 => f(pp, pp2)))

      def many: Parser[List[A]] =
        self.map2(self.many)(_ :: _).or(succeed(List()))

      def many1: Parser[List[A]] = self.map2(self.many)(_ :: _)


    // HELPER FUNCTIONS
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if n <= 0 then succeed(Nil)
      else
        p.map2(listOfN(n - 1, p))(_ :: _)

    def count[A](p: Parser[List[A]]): Parser[Int] = map(p)((as: List[A]) => as.length)

    def other[A, B](p: Parser[A], p2: Parser[B]): Parser[B] =
      for {_ <- p; pp <- p2} yield pp

    def skipWhiteSpace[A](p: Parser[A]): Parser[A] =
      for {
        _ <- whitespace.many
        pp <- p
        _ <- whitespace.many
      } yield pp

    def sequence[A](sep: Parser[Char], p: Parser[A]): Parser[List[A]] =
      for {
        ppl <- p.opt
        next <- other(sep, p).many
      } yield ppl.map(_ :: next).getOrElse(List.empty)



    // COMBINATORS
    val digit: Parser[Int] = regex("""[0-9]""".r).map(_.toInt)  //skipWhiteSpace(regex("""[0-9]""".r).map(_.toInt))
    val letter: Parser[String] = regex("""[a-zA-Z\s]""".r)
    val specials: Parser[String] = regex("""[@\-_:,./=\\(\\)\\*;\\?`'&]""".r)
    val whitespace: Parser[String] = regex("""[\s\t\r\n\f]""".r)
    val number: Parser[String] = digit.many1.slice
    val int: Parser[Int] = digit.many1.slice.map(_.toInt)
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
    val quotedString: Parser[String] = for {
      _ <- char('"')
      v <- (letter | digit | specials).many.slice
      _ <- char('"')
    } yield v
