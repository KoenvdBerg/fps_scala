import scala.io.*
import math.*
import scala.annotation.tailrec
import java.nio.file.{Files, Paths}
import aoc2022.Algorithms.LabelPropagationAlgorithm

object day25 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Map[String, List[String]] =

    def parse(s: String): (String, List[String]) = s match
      case s"$s1: $components" => (s1, components.split(" ").toList)
      case _ => sys.error(s"cannot parse $s")

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(parse).toMap

  def getNodes(in: Map[String, List[String]]): Map[String, List[String]] =

    val togo = (in.keys ++ in.values.flatten).toList.distinct
    val l = togo.length

    @tailrec
    def go(i: Int, res: Map[String, List[String]]): Map[String, List[String]] =
      if i >= l then res
      else
        val s1 = togo(i)
        val nq = togo.patch(i, Nil, 1)
        val cur: List[String] = in.getOrElse(s1, List.empty)
        val finalConnections = nq.foldLeft(cur) { (res: List[String], s2: String) =>
          if in.getOrElse(s2, List.empty).contains(s1) then s2 :: res else res
        }
        go(i+1, res.updated(s1, finalConnections))

    go(0, Map.empty)


  private val wireMap: Map[String, List[String]] = getNodes(input)
  private val labelProp: LabelPropagationAlgorithm[String] = LabelPropagationAlgorithm[String](wireMap, 30, 50)
  private val res1: Map[String, Int] = labelProp.runAndCorrectRNG
  private val answer1: (Map[String, Int], Int) = res1 -> res1.values.product
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  // Original solution: uncomment to write to CSV and to use that CSV in R with the code below:
  // val CSV = "from,to" + "\n" + wireMap.map((w1, to) => to.map(w2 => s"$w1,$w2").mkString("\n")).mkString("\n")
  // Files.write(Paths.get("/home/koenvandenberg/insertdata/scala/fps_scala/aoc2023/graph.csv"), CSV.getBytes(StandardCharsets.UTF_8))

  /**
   * Solve in R-statistics using igraph:
   *
   * library(igraph)
   *
   * dat1 <- read.csv2("/home/koenvandenberg/insertdata/scala/fps_scala/aoc2023/graph.csv", header = TRUE, sep = ",")
   *
   * g <- igraph::graph_from_data_frame(dat1, FALSE)
   *
   * eb2 <- cluster_label_prop(g)
   *
   * mem <- eb2$membership
   *
   * a <- mem[mem == 1]
   * b <- mem[mem == 2]
   * length(a) * length(b)
   *
   */
