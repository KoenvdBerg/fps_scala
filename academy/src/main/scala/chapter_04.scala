object chapter_04{
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail")
    try {
      val x = 42 + 5
      x + y
    } catch { case e: Exception => 43}
  }
  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail")): Int)
    } catch {
      case e: Exception => 43
    }
  }
  def mean_0(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list")
    else
      xs.sum / xs.length

  def mean_1(xs: Seq[Double], onempty: Double): Double =
    if (xs.isEmpty) onempty
    else
      xs.sum / xs.length

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

}

// 4.1
sealed trait KOption[+A]:
  def map[B](f: A => B): KOption[B] = this match
      case KSome(v) => KSome(f(v))
      case KNone => KNone

  def flatMap[B](f: A => KOption[B]): KOption[B] =
    this.map(f).getOrElse(KNone)

  def getOrElse[B >: A](default: => B): B = this match
    case KSome(v) => v
    case KNone => default

  def orElse[B >: A](ob: => KOption[B]): KOption[B] =
    this.map(KSome(_)).getOrElse(ob)

  def filter(f: A => Boolean): KOption[A] =
    this.flatMap(x => if f(x) then KSome(x) else KNone)


case class KSome[+A](get: A) extends KOption[A]
case object KNone extends KOption[Nothing]

// 4.2
import math.*
def mean(xs: Seq[Double]): KOption[Double] =
  if (xs.isEmpty) KNone
  else KSome(xs.sum / xs.length)

def variance(xs: Seq[Double]): KOption[Double] =
  val m = mean(xs)
  val y = m.map(mm => xs.map(x => math.pow(x - mm, 2)))
  y.flatMap(v => mean(v))

def lift[A, B](f: A => B): KOption[A] => KOption[B] = _.map(f)

// 4.3
def map2[A, B, C](a: KOption[A], b: KOption[B])(f: (A, B) => C): KOption[C] =
  a.flatMap(aa => b.flatMap(bb => KSome(f(aa, bb))))

// 4.4
def sequence[A](a: List[KOption[A]]): KOption[List[A]] =
  def go[A](n: Int, acc: KOption[List[A]]): KOption[List[A]] =
    if n >= aa.length then acc
    else
      go(n+1, a, map2(acc, aa(n)((x, y) => )))


@main def C04(): Unit =
  val x = KSome(10)

  println(x.map(_+1))
  println(x.getOrElse(999))
  println(None.getOrElse(999))

  println(x.orElse(KSome(333)))
  println(KNone.orElse(KSome(10101010)))


  println(x.flatMap((d: Int) => KSome(d + 9999)))

  println("### FILTERING ###")
  println(x.filter(_ == 1))
  println(x.filter(_ == 10))

  println("### 4.2 ###")
  val test = List(1.1,2,3,4,5)
  println(mean(test))

  println(variance(test))
  println(variance(Nil))

  println("### 4.3 ###")
  println(map2(KSome(1), KSome(4))((x: Int, y: Int) => x + y))
  println(map2(KSome("Hello"), KSome(" World"))((x, y) => x + y))
  println(map2(KSome("Hello"), KNone)((x, y) => x + y))

  println("### 4.4 ###")
  println(sequence(List(KSome(2), KSome(4))))