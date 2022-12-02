import scala.io.Source
import scala.util.Using

@main def run() : Unit =
  // Name of file
  val filename = "puzzles/puzz2"

  Using(Source.fromFile(filename)) {reader => println(
    // Name of exercise
    ex2A(reader)
  )}

def ex2A(reader: Source) : Int =
  reader.getLines
    .foldLeft(0)((i: Int, c: String) => Tuple.fromArray(c.split(" ").map {
      case "X" | "A" => 1
      case "Y" | "B" => 2
      case "Z" | "C" => 3
    }) match
      case (l: Int, r: Int) => i + r + ((r - l + 4) % 3) * 3
    )

def ex2B(reader: Source) : Int =
  // lose draw win
  reader.getLines
    .foldLeft(0)((i: Int, c: String) => Tuple.fromArray(c.split(" ")) match
      case ("A", v) => (1, v)
      case ("B", v) => (2, v)
      case ("C", v) => (3, v)
      match
        case (v, "X") => i + 0 + ((v + 1) % 3 + 1)
        case (v, "Y") => i + 3 + v
        case (v, "Z") => i + 6 + (v % 3 + 1)
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