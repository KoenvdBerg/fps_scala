object chapter14: 
         
  type ST[S, A] = S => (A, S)
    
  object ST: 
    
    extension [S, A](self: ST[S, A])
      def map[B](f: A => B): ST[S, B] = (s: S) => 
        val (a, s1): (A, S) = self(s)
        (f(a), s1)
        
      def flatMap[B](f: A => ST[S, B]): ST[S, B] = (s: S) => 
        val (a, s1): (A, S) = self(s)
        f(a)(s1)
    
    def apply[S, A](a: => A) =
      lazy val memo: A = a
      (s: S) => (memo, s)

    def lift[S, A](f: S => (A, S)): ST[S, A] = f

    def run[A](st: [s] => () => ST[s, A]): A =
      val su: ST[Unit, A] = st[Unit]()
      su(())(0)
      
  final class STRef[S, A] private (private var cell: A):
    def read: ST[S, A] = ST(cell)
    def write(a : => A): ST[S, Unit] = ST.lift[S, Unit]( (s: S) => {cell = a ; ((), s)})