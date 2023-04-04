@main def encrypt(): Unit = {
  val sn = 7

  val cpk = 5764801
  val dpk = 17807724

  val lsc = determineLoopSize(sn, cpk)
  val lsd = determineLoopSize(sn, dpk)

  val keyc = transform(cpk, lsd.toInt)
  val keyd = transform(dpk, lsc.toInt)

  println(keyc)
  println(keyd)

}


def determineLoopSize(sn: Double, pk: Double): Double = {
  def go(n: Int, acc: Double): Double = {
    if acc == pk then n
    else
      go(n+1, acc * sn % 20201227)
  }
  go(0, 1)
}

def transform(sn: Double, n: Int): Double = {
  def loop(m: Int, acc: Double): Double = {
    println(acc)
    if m == 0 then acc
    else
      val tmp = acc * sn
      loop(m-1, tmp % 20201227)
  }
  loop(n, 1)
}