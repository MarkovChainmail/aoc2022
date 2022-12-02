import jdk.internal.net.http.hpack.Huffman.Reader

import scala.io.Source
import scala.util.Using
@main def run() : Unit =
  // Name of file
  val filename = "puzzles/puzz2"

  Using(Source.fromFile(filename)) {reader => println(
    // Name of exercise
    ex2B(reader)
  )}

def ex2A(reader: Source) : Int =
  // win lose draw
  val w = 6
  val l = 0
  val d = 3

  reader.getLines
    .foldLeft(0)((i: Int, c: String) => Tuple.fromArray(c.split(" ").map {
      case "X" | "A" => 1
      case "Y" | "B" => 2
      case "Z" | "C" => 3
    }) match
      case (1, 1) => i + 1 + d
      case (1, 2) => i + 2 + w
      case (1, 3) => i + 3 + l
      case (2, 1) => i + 1 + l
      case (2, 2) => i + 2 + d
      case (2, 3) => i + 3 + w
      case (3, 1) => i + 1 + w
      case (3, 2) => i + 2 + l
      case (3, 3) => i + 3 + d
    )
def ex2B(reader: Source) : Int =
  // lose draw win
  val x = 0
  val y = 3
  val z = 6

  reader.getLines
    .foldLeft(0)((i: Int, c: String) => Tuple.fromArray(c.split(" ")) match
      case ("A", v) => (1, v)
      case ("B", v) => (2, v)
      case ("C", v) => (3, v)
      match
        case (v, "X") => i + x + ((v + 1) % 3 + 1)
        case (v, "Y") => i + y + v
        case (v, "Z") => i + z + (v % 3 + 1)
    )

def ex1A(reader: Source) : Int =
  reader.getLines
    .foldLeft(List(0))((l: List[Int], c: String) => c match
      case "" => 0 :: l
      case c => c.toInt + l.head :: l.tail
    ).max

def ex1B(reader: Source) : Int =
  reader.getLines
    .foldLeft(List(0))((l: List[Int], c: String) => c match
      case "" => 0 :: l
      case c => c.toInt + l.head :: l.tail
    ).sorted.takeRight(3).sum