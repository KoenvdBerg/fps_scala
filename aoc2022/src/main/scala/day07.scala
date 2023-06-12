import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * The difficult part here is to correctly walk the input file and parse out the file tree. The way I did it is 
 * by keeping track of a state that holds the file tree so far, and the paths to which append new files. See
 * code below
 *
 * The result could be obtained by summing each file in each folder, and then filtering these folder sizes to be below 
 * 100000. 
 *
 * PART 02:
 *
 * Please see code below for part 02. 
 *
 */


object day07 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList

  type FileTree = Map[String, List[(String, Int)]]
  type Paths = List[String]
  
  def walkDir(state: (FileTree, Paths), in: String): (FileTree, Paths) = in match
    case "$ cd .."     => (state._1, state._2.drop(1))  // move 1 dir back
    case s"$$ cd $dir" =>
      val path: String = if state._2.isEmpty then dir else state._2.head + dir  // check for when the root folder hasn't been added yet
      (state._1.updated(path, Nil), path :: state._2)   // move 1 dir deeper and init folder
    case "$ ls"        => state   // ignore and continue
    case s"dir $_"     => state   // ignore and continue
    case s"$i $file"   =>         // add file and size to FileTree for all paths in state._2
      val next: FileTree =
        state._2.foldLeft(state._1)((m: FileTree, key: String) => m.updated(key, (file, i.toInt) :: m(key)))
      (next, state._2)
    case _ => sys.error("BOOM")


  private val (res1, _): (FileTree, Paths) = input.foldLeft((Map.empty[String, List[(String, Int)]], Nil))(walkDir)
  private val answer1: Int = res1
    .map(f => f._2.map(_._2).sum)
    .filter(_ < 100000)
    .sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val totalSpace: Int = res1("/").map(_._2).sum
  private val spaceRequired: Int = 30000000 - (70000000 - totalSpace)
  private val answer2: Int = res1
    .map(f => f._2.map(_._2).sum)
    .filter(_ >= spaceRequired)
    .min
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
