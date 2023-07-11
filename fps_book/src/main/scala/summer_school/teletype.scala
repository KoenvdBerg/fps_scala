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
      extension [A](ta: TeleType[A]) 
        def fmap[B](f: A => B): TeleType[B] = ta match
          case End(a)     => End(f(a))
          case Get(g)     => Get((c: Char) => g(c).fmap(f))
          case Put(c, tn) => Put(c, tn.fmap(f)) 

    given Applicative[TeleType] with
      extension[A] (ta: TeleType[A])
        def pure(a: A): TeleType[A] = End(a)
        def <*>[B](tf: TeleType[A => B]): TeleType[B] = tf match
          case Put(c, tn) => Put(c, ta <*> tn)
          case Get(g)     => Get((c: Char) => ta <*> g(c))
          case End(f)     => ta match
            case End(v)       => End(f(v))
            case Get(g)       => Get((c: Char) => g(c) <*> tf)
            case Put(c2, tn2) => Put(c2, tn2 <*> tf) 

    given Monad[TeleType] with
      extension[A] (ta: TeleType[A])
        def unit(a: A): TeleType[A] = ta.pure(a)
        def flatMap[B](f: A => TeleType[B]): TeleType[B] = ta match
          case End(a)     => f(a)
          case Get(g)     => Get((c: Char) => g(c).flatMap(f))
          case Put(c, tn) => Put(c, tn.flatMap(f))
        def map[B](f: A => B): TeleType[B] = ta.fmap(f)
          
          
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
      
    
    
    
  
  
  
