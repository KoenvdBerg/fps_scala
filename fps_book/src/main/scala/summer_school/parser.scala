package summer_school

import summer_school.AFPLab2.*

object ParserError: 
  
  type ErrorMsg = String
  type Parser[+A] = String => Either[ErrorMsg, (A, String)]
  
  object Parser: 
    
    given Functor[Parser] with
      override def fmap[A, B](fa: Parser[A])(f: A => B): Parser[B] = (s: String) => 
        fa(s) match
          case Right((v, rem)) => Right((f(v), rem))
          case Left(e)         => Left(e)
        
        
    given Applicative[Parser] with
      override def pure[A](a: A): Parser[A] = (s: String) => Right(a -> s) 

      override def ap[A, B](fa: Parser[A])(gf: Parser[A => B]): Parser[B] = (s: String) => 
        gf(s) match
          case Left(e) => Left(e)
          case Right((f, _)) => fa(s) match
            case Left(e) => Left(e)
            case Right((v, rem)) => Right(f(v) -> rem)


    given Monad[Parser] with
      override def bind[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = (s: String) =>
        fa(s) match
          case Right((a, rem)) => f(a)(rem)
          case Left(e)         => Left(e)
    
    
