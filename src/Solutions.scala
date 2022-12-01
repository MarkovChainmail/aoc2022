import scala.io.Source
import scala.util.Using

@main def run() : Unit =
  // Put the one you want to run here
  println(ex1B())

def ex1A() : Int =
  Using(Source.fromFile("puzzles/puzz1")) {reader =>
    reader.getLines().foldLeft(List(0))((l: List[Int], c: String) => c match
      case "" => 0 :: l
      case c => c.toInt + l.head :: l.tail
    )
  }.get.max

def ex1B() : Int =
  Using(Source.fromFile("puzzles/puzz1")) {reader =>
    reader.getLines().foldLeft(List(0))((l: List[Int], c: String) => c match
      case "" => 0 :: l
      case c => c.toInt + l.head :: l.tail
    )
  }.get.sorted.takeRight(3).sum