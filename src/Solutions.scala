import scala.io.Source
import java.nio.file.{Files, Path}
@main def run() : Unit =
  // Put the one you want to run here
  println(ex1B())

def ex1A() : Int =
  Source.fromFile("puzzles/puzz1").getLines
    .foldLeft(List(0))((l: List[Int], c: String) => c match
      case "" => 0 :: l
      case c => c.toInt + l.head :: l.tail
    ).max

def ex1B() : Int =
  Source.fromFile("puzzles/puzz1").getLines
    .foldLeft(List(0))((l: List[Int], c: String) => c match
      case "" => 0 :: l
      case c => c.toInt + l.head :: l.tail
    ).sorted.takeRight(3).sum