import scala.Console.println
import scala.io.Source
import scala.language.postfixOps
import scala.util.Using

@main def run(): Unit =
// Name of file
  val filename = "puzzles/puzz5"

  Using(Source.fromFile(filename)) { reader =>
    println(
      // Name of exercise
      ex5A(reader)
    )
  }

def ex5A(reader: Source): String =
  val Step1 = reader.getLines
    .foldLeft((List[(String, Int)](), List[List[Int]]()))((B, s: String) => s.head match
      case ' ' | '[' => (B(0) ::: {
        s.grouped(4) // Append to the list of boxes
          .map(s => s.filter(c => c.isLetter))
          .zip(LazyList.from(1)).toList // Index tuples at 1
          .filterNot(_._1.isEmpty) // Remove empty tuples
      } , B(1) // Drop the empty space
      )
      case _ => (B(0), {
        println("C2")
        val s1 = s.drop(5) // Append to the list of instructions
          .replace(" from ", " ")
          .replace(" to ", " ")
          .split(" ")
          .toList
        println(s1)
        println("what waaaah")
        val s2 = s1
          .map(_.toInt) +: B(1).toList
        println(s1)
        s2
      }
      )
    )

  println("step reacheed")
  println(Step1)

  // Functionally edit the maps until the final state has been reached
  Step1._2.foldLeft(Step1._1.groupMap(_._2)(_._1))((B: Map[Int, List[String]], instruction: List[Int]) =>
    B ++ List(instruction(1) -> B(instruction(1)).drop(instruction(0)),
      instruction(2) -> (B(instruction(1)).take(instruction(0)).reverse ::: B(instruction(2))))
  ).values.map(_.head).mkString("")



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