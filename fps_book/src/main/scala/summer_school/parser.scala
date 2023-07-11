package summer_school

import summer_school.AFPLab2.*

object ParserError: 
  
  type ErrorMsg = String
  type Parser[+A] = String => Either[ErrorMsg, (A, String)]
  
  object Parser: 
    
    given Functor[Parser] with
      extension [A](p: Parser[A]) 
        def fmap[B](f: A => B): Parser[B] = (s: String) => 
          p(s) match
            case Right((v, rem)) => Right((f(v), rem))
            case Left(e)         => Left(e)
        
        
    given Applicative[Parser] with
      extension [A](p: Parser[A]) 
        def pure(a: A): Parser[A] = (s: String) => Right(a -> s)
        def <*>[B](pf:  Parser[A => B]): Parser[B] = (s: String) => 
          pf(s) match
            case Left(e) => Left(e)
            case Right((f, _)) => p(s) match
              case Left(e) => Left(e)
              case Right((v, rem)) => Right(f(v) -> rem)
        
        
    given Monad[Parser] with 
      extension [A](p: Parser[A])
        def unit(a: A): Parser[A] = p.pure(a)
        def flatMap[B](f: A => Parser[B]): Parser[B] = (s: String) => 
          p(s) match
            case Right((a, rem)) => f(a)(rem)
            case Left(e)         => Left(e)
    
    
