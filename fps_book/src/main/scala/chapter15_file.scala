import cats.effect.{IO, IOApp, ExitCode}



object run_chapter15_file extends IOApp :
  import chapter15.Process

  private val infile: String = "/Users/kvandenberg/Documents/fps_scala/aoc2022/src/main/resources/day05.txt"
  private def transducer[I]: Process[I, Boolean] = Process.count |> Process.exists((i: Int) => i > 500)

  def run(args: List[String]): IO[ExitCode] = 
    args.headOption match
      // case None    => IO.println(s"Please enter an existing file").as(ExitCode(2))
      case _ => (for {
        k <- Process.processFile(infile, transducer, false)(_ || _)
        _ <- IO.println(s"the file check result: $k")
      } yield ()).as(ExitCode.Success)
      
object run_chapter15_fahrenheit extends IOApp:
  import chapter15.Process
  
  val infile: String = "/Users/kvandenberg/tmp"
    
  private def toCelcius(fahrenheit: Double): Double =
    (5.0 / 9.0) * (fahrenheit - 32)
    
  private def program: Process[String, Double] = Process.lift((s: String) => s.toDouble) |> Process.lift(toCelcius)

  def run(args: List[String]): IO[ExitCode] =
    args.headOption match
      case _ => (for {
        k <- Process.processFile(infile, program, Nil)((b: List[Double], a: Double) => a :: b)
        _ <- IO.println(s"the file check result: $k")
      } yield ()).as(ExitCode.Success)
  