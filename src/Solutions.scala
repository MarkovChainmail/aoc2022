import jdk.internal.net.http.hpack.Huffman.Reader

import scala.io.Source
import scala.util.Using
@main def run() : Unit =
  // Name of file
  val filename = "puzzles/puzz1"

  Using(Source.fromFile(filename)) {reader => println(
    // Name of exercise
    ex1B(reader)
  )}

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