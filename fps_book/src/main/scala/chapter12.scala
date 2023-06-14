import chapter11.Functor
import chapter11.Monad
import chapter11.Monad.{Id, idMonad}
import chapter06_state.State

object chapter12: 
  trait Applicative[F[_]] extends Functor[F]: 
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ???
    def unit[A](a: => A): F[A] = ???
    def map[A, B](fa: F[A])(f: A => B): F[B] = ???
    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List.empty[B]))((a: A, fbs: F[List[B]]) => map2(f(a), fbs)(_ :: _))
    def sequence[A](as: List[F[A]]): F[List[A]] =
      traverse(as)(identity)

    // 12.12
    def sequenceMap[K, V](as: Map[K, F[V]]): F[Map[K, V]] =
      as.foldRight(unit(Map.empty[K, V]))((kv: (K, F[V]), fkv: F[Map[K, V]]) => map2(kv._2, fkv)((a: V, b: Map[K, V]) => b + (kv._1 -> a)))
      
    // 12.1
    def **[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      sequence(List.fill(n)(ma))
      
    // 12.2 
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((ga: A => B, a: A) => ga(a))
    def map2Dos[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(mapDos(fa)(f.curried))(fb) 
    def mapDos[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
    
    // 12.3
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(mapDos(fa)(f.curried))(fb))(fc)

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(mapDos(fa)(f.curried))(fb))(fc))(fd)
      
    // 12.8
    def product[G[_]](g: Applicative[G]): Applicative[[x] =>> (F[x], G[x])] =
      val self = this
      new Applicative[[x] =>> (F[x], G[x])]:
        override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), g.unit(a))
        override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
          (self.apply(fab._1)(fa._1), g.apply(fab._2)(fa._2))
    
    // 12.9
    def compose[G[_]](g: Applicative[G]): Applicative[[x] =>> F[G[x]]] =
      val self = this
      new Applicative[[x] =>> F[G[x]]]:
        override def unit[A](a: => A): F[G[A]] = self.unit(g.unit(a))
        override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
          self.map2(fa, fb)((ga: G[A], gb: G[B]) => g.map2(ga, gb)(f))
      
  object Applicative: 
    
    val streamApplicative: Applicative[LazyList] = new Applicative[LazyList]:
      override def unit[A](a: => A): LazyList[A] = LazyList.continually(a)
      override def map2[A, B, C](fa: LazyList[A], fb: LazyList[B])(f: (A, B) => C): LazyList[C] = fa.zip(fb).map(f.tupled)
      
    val idApplicative: Applicative[Id] = new Applicative[Id]:
      override def unit[A](a: => A): Id[A] = idMonad.unit(a)
      override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = idMonad.map2(fa, fb)(f) 
      
    def stateApplicative[S] = new Applicative[[x] =>> State[S, x]]:
      override def map2[A, B, C](fa: State[S, A], fb: State[S, B])(f: (A, B) => C): State[S, C] = fa.map2(fb)(f)
      override def unit[A](a: => A): State[S, A] = State.unit(a)

  // 12.5
  enum Either[+E, +A]:
    case Left(value: E)
    case Right(value: A)

  object Either: 
    def EiterMonad[E] = new Monad[[x] =>> Either[E, x]]:
      override def unit[A](a: A): Either[E, A] = Right(a)
      override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match
        case Right(v) => f(v)
        case Left(e)  => Left(e)
        
  // 12.6
  enum Validation[+E, +A]: 
    case Failure(head: E, tail: Vector[E] = Vector.empty)
    case Success(a: A)
    
  object Validation: 
    def validationApplicative[E] = new Applicative[[x] =>> Validation[E, x]]:
      override def unit[A](a: => A): Validation[E, A] = Success(a)
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, (h2 +: t1) ++ t2)
        case (_, Failure(h, t)) => Failure(h, t)
        case (Failure(h, t), _) => Failure(h, t)
        
        
  trait Traverse[F[_]]: 
    val self = this
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = ???
    def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)
    def map[A, B](fa: F[A])(f: A => B)(using g: Applicative[Id]): F[B] = traverse(fa)((a: A) => g.unit(f(a))).value
    def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
      traverse(fa)(f)(Applicative.stateApplicative)
      
    def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
      traverseS(fa)((a: A) => for { 
        s1 <- State.get[S]
        (b, s2) = f(a, s1)
        _ <- State.set(s2)
      } yield b).run(s)
      
    def toList[A](fa: F[A]): List[A] =
      mapAccum(fa, List.empty[A])((a: A, s: List[A]) => ((), a :: s))._2.reverse
      
    def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
      mapAccum(fa, 0)((a: A, s: Int) => ((a, s), s + 1))._1
    
    // 12.16
    def reverse[A](fa: F[A]): F[A] =
      mapAccum(fa, toList(fa).reverse)((_: A, s: List[A]) => (s.head, s.tail))._1
  
    // 12.17
    def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
      mapAccum(fa, z)((a: A, s: B) => ((), f(s, a)))._2
      
    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      mapAccum(fa, toList(fb)) {
        case (_, Nil)    => sys.error("zip: Incompatible shapes. ")
        case (a, b :: s) => ((a, b), s) 
      }._1


    def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
      mapAccum(fa, toList(fb)) {
        case (a, Nil) => ((a, None), Nil)
        case (a, b :: s) => ((a, Some(b)), s)
      }._1


    def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
      mapAccum(fb, toList(fa)) {
        case (b, Nil) => ((None, b), Nil)
        case (b, a :: s) => ((Some(a), b), s)
      }._1

    // 12.18
    def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(using G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
      traverse[[x] =>> (G[x], H[x]), A, B](fa)((a: A) => (f(a), g(a)))(G.product(H))
      
    // 12.19
    //def compose[G[_]](using G: Traverse[G]): Traverse[[x] =>> F[G[x]]] = new Traverse[[x] =>> F[G[x]]]:
    // override def traverse[M[_] : Applicative, A, B](fa: F[G[A]])(f: A => M[B]): F[G[M[B]]] = 
    //    self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    
    
    
  object Traverse: 
    // 12.13
    val listTraverse: Traverse[List] = new Traverse[List]:
      override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(using g: Applicative[G]): G[List[B]] =
        fa.foldRight(g.unit(List.empty[B]))((a: A, fbs: G[List[B]]) => g.map2(f(a), fbs)(_ :: _))
    
    val optionTraverse: Traverse[Option] = new Traverse[Option]:
      override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(using g: Applicative[G]): G[Option[B]] = fa match
        case Some(v) => g.map(f(v))(Some(_))
        case None    => g.unit(None)
        
    case class Tree[+A](head: A, tail: List[Tree[A]])
    val treeTraverse: Traverse[Tree] = new Traverse[Tree]:
      override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(using g: Applicative[G]): G[Tree[B]] =
        g.map2(f(fa.head), listTraverse.traverse(fa.tail)((a: Tree[A]) => traverse(a)(f)))((b: B, bt: List[Tree[B]]) => Tree(b, bt))


      
@main def run_chapter12: Unit =

  import chapter12.Applicative.streamApplicative.*
  val a: LazyList[Option[String]] = unit(Some("KOEN"))
  val b: LazyList[Int] = unit(1)
      
  println(sequence(List(a, b)).take(4).toList)

  /**
   * 12.4 
   * The meaning of the `sequence` method for the Stream (i.e. LazyList) is that the original list that contains multiple
   * streams will now become 1 stream. Each item in that stream will be a list of the items from the original stream. 
   * 
   * Why is this useful? Well for example in the case to combine multiple infitive data streams in stream processing. 
   */
  
  // testing Either Monad
  import chapter12.Either
  
  val x: Either[String, Int] = Either.EiterMonad.unit(100)
  println(Either.EiterMonad.flatMap(x)((i: Int) => Either.EiterMonad.unit(i / 10 + 9)))
  
  // testing Validation applicative
  import chapter12.Validation
  val y1: Validation[Nothing, Int] = Validation.validationApplicative.unit(100)
  val y2: Validation[String, Nothing] = Validation.Failure("kdlsj", Vector("ERROR1"))
  val y3: Validation[String, Nothing] = Validation.Failure("I'm jeff", Vector("ERROR2"))
  println(Validation.validationApplicative.map2(y1, y2)((a, b) => (a, b)))
  println(Validation.validationApplicative.map2(y2, y3)((a, b) => (a, b)))
  


