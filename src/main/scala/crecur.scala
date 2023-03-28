//def fib(n: Int): Int = {
//  def go(a: Int, b: Int, n: Int): Int = {
//    if n <= 0 then a
//    else go(b, a+b, n-1)
//  }
//  go(0, 1, n)
//}
//
//def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
//  def loop(n: Int): Boolean = {
//    if n + 1 >= as.length then true
//    else if ordered(as(n), as(n+1)) then loop(n+1)
//    else false
//  }
//  loop(0)
//}
//
//def check(a: Int, b: Int): Boolean = {
//  if a < b then true
//  else false
//}
//
//def checkSame(a: Int, b: Int): Boolean = {
//  if a == b then true
//  else false
//}
//
//isSorted(Array(1,2,3,4,5,6,7), check)
//
//
//// FROM THE LINK SENT BY MARCO:
//// https://alvinalexander.com/scala/scala-recursion-examples-recursive-programming/
//
//// SUM
//def sumList(ls: List[Int]): Int = {
//  def go(ls: List[Int], acc: Int): Int = {
//    ls match {
//      case Nil => acc
//      case c :: tail => go(tail, acc + c)
//    }
//  }
//  go(ls, 0)
//}
//
//// PRODUCT
//def prodList(ls: List[Int]): Int = {
//  def go(ls: List[Int], acc: Int): Int = {
//    ls match {
//      case Nil => acc
//      case cons :: tail => go(tail, cons * acc)
//    }
//  }
//  go(ls, 1)
//}
//
//// MAX
//def maxList(ls: List[Int]): Int = {
//  def go(ls: List[Int], acc: Int): Int = {
//    ls match {
//      case Nil => acc
//      case cons :: tail => {
//        if (cons > acc) go(tail, cons)
//        else go(tail, acc)
//      }
//    }
//  }
//  go(ls, -1)
//}
//
//// FIB
//def fib(n: Int): Int = {
//  def go(a: Int, b: Int, n: Int): Int = {
//    if (n <= 0) a
//    else go(b, a+b, n-1)
//  }
//  go(0, 1, n)
//}
//
//// PRINT FIB
//def printFib(a: Int, b: Int): Unit = {
//  val c = a + b
//  print(s" $c ")
//  if (c > 1000000) println("DONE")
//  else printFib(b, c)
//}
//
//// FACTORIAL
//def fact(n: Int): BigInt = {
//  def go(n: Int, acc: Int): BigInt = {
//    if (n <= 1) acc
//    else go(n-1, acc*n)
//  }
//  go(n, 1)
//}
//
//
//def sum(ints: List[Int]): Int = ints match {
//  case Nil => 0
//  case x :: tail => x + sum(tail)
//}