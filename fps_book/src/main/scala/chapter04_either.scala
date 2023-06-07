sealed trait KEither[+E, +A]:
  def map[B](f: A => B): KEither[E, B] = this match
    case KRight(a) => KRight(f(a))
    case KLeft(e) => KLeft(e)
  def flatMap[EE >: E, B](f: A => KEither[EE, B]): KEither[EE, B] = this match
    case KRight(a) => f(a)
    case KLeft(e) => KLeft(e)
  def orElse[EE >: E, B >: A](b: => KEither[EE, B]): KEither[EE, B] = this match
    case KRight(a) => KRight(a)
    case KLeft(_) => b
  def map2[EE >: E, B, C](b: KEither[EE, B])(f: (A, B) => C): KEither[EE, C] =
    // this.flatMap(aa => b.map(bb => f(aa, bb)))
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
case class KLeft[+E](value: E) extends KEither[E, Nothing]
case class KRight[+A](value: A) extends KEither[Nothing, A]


object KEither {
  def mean(xs: IndexedSeq[Double]): KEither[String, Double] =
    if xs.isEmpty then KLeft("mean of empty list")
    else KRight(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): KEither[Exception, Int] =
    Try(x / y)

  def Try[A](a: => A): KEither[Exception, A] =
    try KRight(a)
    catch { case e: Exception => KLeft(e) }

  // 4.7
  def traverse[E, A, B](as: List[A])(f: A => KEither[E, B]): KEither[E, List[B]] = as match
//    case h :: t => f(h).flatMap(hh => traverse(t)(f).map(hh :: _))
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    case Nil => KRight(Nil)

  def sequence[E, A](es: List[KEither[E, A]]): KEither[E, List[A]] =
    traverse(es)((f: KEither[E, A]) => f)


  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)


  // 4.8
  def mkName(name: String): KEither[String, Name] =
    if name == "" | name == null then KLeft("Name is empty")
    else KRight(new Name(name))

  def mkAge(age: Int): KEither[String, Age] =
    if age < 0 then KLeft("Age is out of range")
    else KRight(new Age(age))

  def mkPerson(name: String, age: Int): KEither[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  // To improve this, I would make the error type of Either of type List, so that all the
  // errors that are found can be appended to that list.
}


@main def run_chapter04_either(): Unit =
  val x = Vector(1.1,2.2,4,5,20)
  println(KEither.mean(x))
  println(KEither.mean(Vector()))
  println(KEither.safeDiv(10,5))
  println(KEither.safeDiv(10,0))

  println("### 4.6 ###")
  val tr = KRight(33)
  val tl = KLeft("Multiplication impossible")
  println(tr.map(_ * 33))
  println(tl.map(o => 0 * 33))

  println(tr.orElse(KRight(100)))
  println(tl.orElse(KRight(100)))

  println(tr.flatMap(o => KRight(o * 100)))
  println(tr.flatMap(o => KLeft(KEither.Try(o / 0))))

  val mres = KLeft("skdfks").map2(KRight(100))((a: Int, b: Int) => a * b)
  val mres2 = KRight(999).map2(KRight(100))((a: Int, b: Int) => a * b)
  println(mres)
  println(mres2)

  println("### 4.7 ###")
  val z = List(1,2,3,4,5)
  val res1 = KEither.traverse(z)(f => KEither.Try(10 / f))
  println(res1)

  val u = List(1,2,0,3,4,5)
  val res2 = KEither.traverse(u)(f => KEither.Try(100 / f))
  println(res2)

  val s = List(KRight(1), KRight(4), KRight(9))
  val s2 = List(KRight(1), KRight(4), KRight(9), KLeft(2))
  println(KEither.sequence(s))
  println(KEither.sequence(s2))

  println("### 4.8 ###")
  println(KEither.mkName("bob"))
  println(KEither.mkName(""))
  println(KEither.mkPerson("bob", 40))
  println(KEither.mkPerson("", -10))