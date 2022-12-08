import java.security.KeyStore.TrustedCertificateEntry
import scala.Console.println
import scala.io.Source
import scala.language.postfixOps
import scala.util.Using

@main def run(): Unit =
// Name of file
  val filename = "puzzles/puzz7"

  Using(Source.fromFile(filename)) { reader =>
    println(
      // Name of exercise
      ex7A(reader)
    )
  }

def ex8A(reader: Source) =
  val array = reader.getLines
    .map(_.toArray.map(_.toString.toInt))
    .toArray // 2D Array

  array.indices.flatMap(x => array(x).indices.map((x, _))) // iterate over all indices
    .map((x, y) =>
      val current = array(x)(y)
      Vector(
        (0 until x).exists(array(_)(y) >= current),
        (x+1 until array.length).exists(array(_)(y) >= current),
        (0 until y).exists(array(x)(_) >= current),
        (y+1 until array(x).length).exists(array(x)(_) >= current)
      ).contains(false)
    ).count(x => x)

def ex8B(reader: Source) =
  val array = reader.getLines
    .map(_.toArray.map(_.toString.toInt))
    .toArray // 2D Array

  def takeWhilePlusFailure(i: Iterable[Int], current: Int) =
    val (prefix, postfix) = i.span(_ < current)
    (prefix ++ postfix.take(1)).toList.length


  array.indices.flatMap(x => array(x).indices.map((x, _))) // iterate over all indices
    .map((x, y) =>
      val current = array(x)(y)

      val l = takeWhilePlusFailure((x-1 to 0 by -1).map(array(_)(y)), current)
      val r =  takeWhilePlusFailure((x+1 until array.length).map(array(_)(y)), current)
      val d =  takeWhilePlusFailure((y-1 to 0 by -1).map(array(x)(_)), current)
      val u =  takeWhilePlusFailure((y+1 until array(x).length).map(array(x)(_)), current)

      l*r*d*u
    ).max

// unfinished
def ex7A(reader: Source) =
  // Define some data structures
  case class Folder(parent: String, children: Set[String], size: Int)
  case class FileSystem(path: List[String], hierarchy: Map[String, Folder]) {
    def newFolder(name: String): FileSystem =
      val uniq = (name :: path).mkString("/")
      if (!(hierarchy contains uniq))
        val newfolder : Map[String, Folder] = if (path.isEmpty)
          // When there's no path, create a root folder
          Map(uniq -> Folder("", Set(), 0))
        else
          // When there is a path, add to the hierarchy
          Map(
            uniq -> Folder(path.mkString("/"), Set(), 0),
            path.mkString("/") -> hierarchy(path.mkString("/")).copy(children = hierarchy(path.mkString("/")).children + uniq)
          )
        this.copy(hierarchy = hierarchy ++ newfolder)
      else
        this

    def addFile(size: Int): FileSystem =
      val oldf = hierarchy(path.mkString("/"))
      val newf = (path.mkString("/"), oldf.copy(size = oldf.size + size))
      this.copy(hierarchy = hierarchy + newf)

    def downwards(name: String): FileSystem =
      newFolder(name).copy(path = name :: path)

    def upwards(): FileSystem =
      this.copy(path = path.tail)

    def dirSize(name: String): Int =
      hierarchy(name).children.map(dirSize).sum + hierarchy(name).size

    def sum(lessThan: Int) =
      hierarchy
      .keys
      .map(dirSize)
      .filter(_ <= lessThan).sum
  }

  // Really should've used mutables here. The temptation of purism is strong...
  val fs = reader.getLines.foldLeft(FileSystem(List(), Map()))((fs: FileSystem, c: String) =>
    c.split(" ").toList match
      case "$" :: "cd" :: tail => tail.head match // Update the current path
        case ".." => fs.upwards()
        case res : String => fs.downwards(res)
      case "$" :: "ls" :: _ => fs // Nothing to change here
      case "dir" :: _ => fs //.newFolder(name)
      case num :: _ => fs.addFile(num.toInt)
    )

  fs.sum(100000)

def ex6A(reader: Source): Int =
  reader.mkString
    .sliding(4,1)
    .zipWithIndex
    .find((s: String, _) => s.distinct.length == 4) match {case Some((_, i: Int)) => i+4}

def ex6B(reader: Source): Int =
  reader.mkString
    .sliding(14,1)
    .zipWithIndex
    .find((s: String, _) => s.distinct.length == 14) match {case Some((_, i: Int)) => i+14}

def ex5A(reader: Source): String =
  val (initial_unparsed, steps_unparsed) = reader.getLines.span(_.take(1) match
    case " " | "[" | "" => true
    case _ => false
  )

  // Traverse bottom to top, drop out empty line
  val initial_partial = initial_unparsed.toList.reverse.drop(1)

  // Don't use more than 9 columns lol
  val lines = initial_partial.head.filter(_.isDigit).toList.map(_.toString.toInt).max

  val empty: List[List[String]] = List.fill(lines)(List())
  //Fill out an empty list of lists
  val initial_state_matrix : List[List[String]] = initial_partial.drop(1)
    .foldLeft(empty)((l : List[List[String]], s) =>
      s.drop(1)
        .sliding(1, 4)
        .zip(l)
        .map((box, stack: List[String]) => if (box != " ") box :: stack else stack)
        .toList
    )

  //Parse the steps and apply them
  val steps = steps_unparsed
    .map(_.split("\\D+").filter(_.nonEmpty).map(_.toInt).toList)// Turn into list of numbers

  steps.foldLeft(initial_state_matrix)((l, step) =>
        // Update with new positions
        l.updated(step(1)-1,l(step(1)-1).drop(step(0))) // Take from fist position
          .updated(step(2)-1, l(step(1)-1).take(step(0)).reverse ++ l(step(2)-1)) // Add onto second position
  ).map(_.head).mkString("")

def ex5B(reader: Source): String =
  val (initial_unparsed, steps_unparsed) = reader.getLines.span(_.take(1) match
    case " " | "[" | "" => true
    case _ => false
  )

  // Traverse bottom to top, drop out empty line
  val initial_partial = initial_unparsed.toList.reverse.drop(1)

  // Don't use more than 9 columns lol
  val lines = initial_partial.head.filter(_.isDigit).toList.map(_.toString.toInt).max

  val empty: List[List[String]] = List.fill(lines)(List())
  //Fill out an empty list of lists
  val initial_state_matrix : List[List[String]] = initial_partial.drop(1)
    .foldLeft(empty)((l : List[List[String]], s) =>
      s.drop(1)
        .sliding(1, 4)
        .zip(l)
        .map((box, stack: List[String]) => if (box != " ") box :: stack else stack)
        .toList
    )

  //Parse the steps and apply them
  val steps = steps_unparsed
    .map(_.split("\\D+").filter(_.nonEmpty).map(_.toInt).toList)// Turn into list of numbers

  steps.foldLeft(initial_state_matrix)((l, step) =>
    // Update with new positions
    l.updated(step(1)-1,l(step(1)-1).drop(step(0))) // Take from fist position
      .updated(step(2)-1, l(step(1)-1).take(step(0)) ++ l(step(2)-1)) // Add onto second position
  ).map(_.head).mkString("")


def ex4A(reader: Source): Int =
  reader.getLines
    .map(s => s.split(",")) // Parse input into 2 pairs of numbers
    .map(s => s.map(c => c.split("-").map(i => i.toInt)))
    .count(s => (s(0)(0).compare(s(1)(0)) + s(0)(1).compare(s(1)(1))).abs < 2) // Compare boundaries

def ex4B(reader: Source): Int =
  reader.getLines
    .map(s => s.split(",")) // Parse input into 2 pairs of numbers
    .map(s => s.map(c => c.split("-").map(i => i.toInt)))
    .count(s => (s(0)(0).compare(s(1)(1)) + s(0)(1).compare(s(1)(0))).abs < 2) // Compare boundaries (slightly differently)

def ex3A(reader: Source): Int =
  reader.getLines
    .map(s => s.slice(0, s.length / 2).intersect(s.slice(s.length / 2, s.length)).head) // Get the element in common
    .map(c => if (c.isLower) c - 96 else c - 64 + 26) // ASCII Conversions to get the numerical value
    .sum // Sum

def ex3B(reader: Source): Int =
  reader.getLines
    .grouped(3)
    .map(g => g(0).intersect(g(1)).intersect(g(2)).head) // Get the element in common
    .map(c => if (c.isLower) c - 96 else c - 64 + 26) // ASCII Conversions to get the numerical value
    .sum // Sum

def ex2A(reader: Source): Int =
  reader.getLines
    .foldLeft(0)((i: Int, c: String) => Tuple.fromArray(c.split(" ").map {
      case "X" | "A" => 1 // Convert all letters to values of move
      case "Y" | "B" => 2
      case "Z" | "C" => 3
    }) match
      case (l: Int, r: Int) => i + r + ((r - l + 4) % 3) * 3 // Compute match outcome
    )

def ex2B(reader: Source): Int =
  reader.getLines
    .foldLeft(0)((i: Int, c: String) => Tuple.fromArray(c.split(" ")) match
      case ("A", v) => (1, v) // (Point value of enemy move, match outcome)
      case ("B", v) => (2, v)
      case ("C", v) => (3, v)
      match
        case (v, "X") => i + 0 + ((v + 1) % 3 + 1) // Compute the correct move for desired outcome
        case (v, "Y") => i + 3 + v
        case (v, "Z") => i + 6 + (v % 3 + 1)
    )

def ex1A(reader: Source): Int =
  reader.getLines
    .foldLeft(List(0))((l: List[Int], c: String) => c match
      case "" => 0 :: l // Add a new head on newline
      case c => c.toInt + l.head :: l.tail // Increment the head
    ).max // Max

def ex1B(reader: Source): Int =
  reader.getLines
    .foldLeft(List(0))((l: List[Int], c: String) => c match
      case "" => 0 :: l // Add a new head on newline
      case c => c.toInt + l.head :: l.tail // Increment the head
    ).sorted.takeRight(3).sum // Top 3