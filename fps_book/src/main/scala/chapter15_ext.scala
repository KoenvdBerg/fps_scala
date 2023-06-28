object chapter15_ext:

  enum Process[F[_], O]: 
    case Await[A](req: F[A], recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]
    case Emit(head: O, tail: Process[F, O]) extends Process[F, O]
    case Halt(err: Throwable)
    
  object Process: 
    
    case object End extends Exception
    case object Kill extends Exception