
object run_chapter02 {
  def abs(n: Int): Int = {
    if (n < 0) then -n
    else n
  }

  def formatResult(name: String, x: Int, f: Int => Int): String = {
    val msg = s"The $name of ${x} is ${f(x)}"
    msg
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("abs", -42, abs))
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if n <= 0 then acc
      else go(n-1, acc*n)
    }
    go(n, 1)
  }

  // 2.1
  def fibbonaci(n: Int): Int = {
    def go(a: Int, b: Int, n: Int): Int = {
      if n == 0 then a
      else
        go(b, a+b, n-1)
    }
    go(0, 1, n)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if n >= as.length then -1
      else if p(as(n)) then n
      else loop(n + 1)
    }
    loop(0)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if n+1 >= as.length then true
      else if !ordered(as(n), as(n + 1)) then false
      else
        loop(n+1)
    }
    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a,b)
  }

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    (a: A) => (b: B) => f(a,b)
  }

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // 2.5
    def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
  //val f = compose((y: Int) => y * 2, (x: Int) => x + 1)
}


