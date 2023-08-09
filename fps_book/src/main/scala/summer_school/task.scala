import scala.annotation.tailrec

object task: 
  enum Task[+A]:
    case Succeed(make: () => A)
    case FlatMap[A, B](task: Task[A], andThen: A => Task[B]) extends Task[B]
  
    def map[B](f: A => B): Task[B] = this.flatMap(a => Task.succeed(f(a)))
    def flatMap[B](f: A => Task[B]): Task[B] = FlatMap(this, f)
  
  object Task:
  
    def succeed[A](a: => A): Task[A] = Succeed(() => a)
  
    def excecute[A](task: Task[A]): A =
  
      @tailrec
      def loop(task: Task[Any], stack: List[Any => Task[Any]]): Any = task match
        case Succeed(value) => stack match
          case Nil => value()
          case h :: t => loop(h(value()), t)
        case FlatMap(t, at) => loop(t, at.asInstanceOf[Any => Task[Any]] :: stack)
  
      loop(task.asInstanceOf[Task[Any]], Nil).asInstanceOf[A]