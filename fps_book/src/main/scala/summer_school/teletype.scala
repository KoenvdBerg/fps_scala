package summer_school

import summer_school.AFPLab2.*
import cats.data.State
import cats.effect.{IO, IOApp}

object teletype extends IOApp.Simple:

  enum TeleType[+A]: 
    case End(v: A)
    case Get(f: Char => TeleType[A])
    case Put(c: Char, ta: TeleType[A])


  object TeleType:

    def getLine: TeleType[String] = Get((c: Char) => if c == '\n' then End(s"$c") else Put(c, getLine))
    
    given Functor[TeleType] with
      override def fmap[A, B](fa: TeleType[A])(f: A => B): TeleType[B] = fa match  
        case End(a)     => End(f(a))
        case Get(g)     => Get((c: Char) => fmap(g(c))(f))
        case Put(c, tn) => Put(c, fmap(tn)(f)) 

    given Applicative[TeleType] with
      override def pure[A](a: A): TeleType[A] = End(a)
      override def ap[A, B](fa: TeleType[A])(gf: TeleType[A => B]): TeleType[B] = gf match
        case Put(c, tn) => Put(c, ap(fa)(tn))
        case Get(g)     => Get((c: Char) => ap(fa)(g(c)))
        case End(f)     => fa match
          case End(v)       => End(f(v))
          case Get(g)       => Get((c: Char) => ap(g(c))(gf))
          case Put(c2, tn2) => Put(c2, ap(tn2)(gf)) 
    
    given Monad[TeleType] with
      override def bind[A, B](fa: TeleType[A])(f: A => TeleType[B]): TeleType[B] = fa match
        case End(a)     => f(a)
        case Get(g)     => Get((c: Char) => bind(g(c))(f))
        case Put(c, tn) => Put(c, bind(tn)(f))

          
    def getChar: TeleType[Char] = Get((c: Char) => End(c))
    def putChar(c: Char): TeleType[Unit] = Put(c, End(()))
    
    def main: TeleType[Char] = for {
      x <- getChar
      _ <- putChar(x)
      _ <- main
    } yield x 
    
    // Below type is different from usual State type in that the S type in this case also is a Monad. So essentially 
    // it forms a Monad within a Monad. 
    type MonadState[A] = State[TeleType[A], A]
    extension [A](ts: MonadState[A]) 
      def unit(a: A): MonadState[A] = State.pure(a)
      def flatMap[B](f: A => MonadState[B]): MonadState[B] = ???
      
    def runConsole[A](ta: TeleType[A]): IO[A] = ta match
        case End(a) => IO.pure(a)
        case Get(f) => for {
          r <- IO.readLine.map(_.head)
          v <- runConsole(f(r))
        } yield v
        case Put(c, tn) => for {
          _ <- IO.println(c)
          v <- runConsole(tn)
        } yield v
  
  end TeleType
         
  def run  = for {
    _ <- TeleType.runConsole(TeleType.getLine)
  } yield ()
      
    
    
    
  
  
  
