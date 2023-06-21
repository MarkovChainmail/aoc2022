import java.security.KeyStore.TrustedCertificateEntry
import scala.Console.println
import scala.io.Source
import scala.language.postfixOps
import scala.util.{Try, Using}
import scala.collection.mutable.{ListBuffer, Map as MutableMap}
import scala.collection.immutable.Queue

@main def run(): Unit =
// Name of file
  val filename = "puzzles/testy"

  Using(Source.fromFile(filename)) { reader =>
    println(
      // Name of exercise
      ex18A(reader)
    )
  }

def ex18A(reader: Source) =
  val cubes = reader.getLines.map(_.split(",").map(_.toInt).toList).toList
  cubes.map(
    c => ((0 to 2).map(i => c.updated(i,c(i)+1)) ++ (0 to 2).map(i => c.updated(i,c(i)-1)))
        .map(neighbor => if (!cubes.contains(neighbor)) 1 else 0)
        .sum
  ).sum

// TOO LOW
def ex18B(reader: Source) =
  val cubes = reader.getLines.map(_.split(",").map(_.toInt).toList).toSet

  // Get the minimal and maximal value
  val xs = cubes.map(_(0))
  val ys = cubes.map(_(1))
  val zs = cubes.map(_(2))

  val (xmin, xmax, ymin, ymax, zmin, zmax) = (xs.min, xs.max, ys.min, ys.max, zs.min, zs.max)
  val depth = List(xmax-xmin, ymax-ymin, zmax-zmin).max

  // Create an outer layer of air and work your way inwards
  val outerAir = (
    (xmin to xmax).flatMap(x => (ymin to ymax).flatMap(y => List(List(x,y,zmin-1), List(x,y,zmax+1)))) ++
    (xmin to xmax).flatMap(x => (zmin to zmax).flatMap(z => List(List(x,ymin-1,z), List(x,ymax+1,z)))) ++
    (ymin to ymax).flatMap(y => (zmin to zmax).flatMap(z => List(List(xmin-1,y,z), List(xmax+1,y,z))))
    ).toSet


  val allAir = (0 to depth).foldLeft(outerAir)((air, d) =>
    // Generate a new layer of air molecules
    air ++ (
      (xmin+d to xmax-d).flatMap(x => (ymin+d to ymax-d).flatMap(y => List(List(x, y, zmin+d), List(x, y, zmax-d)))) ++
      (xmin+d to xmax-d).flatMap(x => (zmin+d to zmax-d).flatMap(z => List(List(x, ymin+d, z), List(x, ymax-d, z)))) ++
      (ymin+d to ymax-d).flatMap(y => (zmin+d to zmax-d).flatMap(z => List(List(xmin+d, y, z), List(xmax-d, y, z))))
    ).toSet

      // Is potentially air?
      .diff(cubes)
      // Has a neighbor in the air?
      .filter {
        case List(x, y, z) =>
          List(
            List(x-1,y,z),List(x+1,y,z),List(x,y-1,z),List(x,y+1,z),List(x,y,z-1),List(x,y,z+1)
          ).exists(air.contains)
      }
  )

  // Count all air molecules that have a surface neighbor
  allAir.toList.flatMap(
    c => ((0 to 2).map(i => c.updated(i, c(i) + 1)) ++ (0 to 2).map(i => c.updated(i, c(i) - 1)))
  ).count(cubes.contains)

def ex17A(reader: Source) =
  val rocks = List(
    List((0,0),(1,0),(2,0),(3,0)),
    List((1,0),(0,1),(1,1),(2,1),(1,2)),
    List((0,0),(1,0),(2,0),(2,1),(2,2)),
    List((0,0),(0,1),(0,2),(0,3)),
    List((0,0),(0,1),(1,0),(1,1))
  )

  object Itr {
    val template: String = reader.mkString
    var index = 0

    def next: Char =
      val currentIndex = index
      index = (index+1)%(template.length-1)
      template(currentIndex)
  }

  // Fold over the rocks to get a terrain coordinate map
  (0 until 2022).foldLeft((0 to 6).map((_,0)).toSet)((terrain:Set[(Int,Int)], i:Int) =>

    // Initial position
    var rock: List[(Int, Int)] = rocks(i % 5)
    val highest: Int = terrain.maxBy(_._2)._2

      // Set rock in its place
    rock = rock.map((x,y) => (x+2, y+4+highest))

    // Heighten the walls
    var falling = true
    while (falling) {
      val nxt = Itr.next
      //println(nxt)

      // Sideways wind
      val newRock = nxt match
        case '<' => rock.map((x,y) => (x-1, y))
        case '>' => rock.map((x,y) => (x+1, y))

      if (terrain.intersect(newRock.toSet).isEmpty && !newRock.exists((x,_) => 0 > x || x > 6))
        rock = newRock

      // Downwards fall
      val newRockDown = rock.map((x,y) => (x,y-1))
      if (terrain.intersect(newRockDown.toSet).nonEmpty)
        falling = false
      else
        rock = newRockDown
    }

    val newTerrain = terrain ++ rock
    // Filter out the lower sections of the tower
    val bottoms = newTerrain.groupBy(_._1).values.map(_.map(_._2).max)
    //println(bottoms)
    val bottom = bottoms.min
    newTerrain.filter(_._2 >= bottom)
  ).maxBy(_._2)._2


def ex14A(reader: Source) =
  val origin = (500,0)

  val rock = reader.getLines.map(_.split(" -> ").toList
    .map(_.split(",").map(_.toInt).toList).map({case List(x,y) => (x,y)}))
    .flatMap(l =>
        l.zip(l.drop(1)).map(c =>
          if (c(0)(0) == c(1)(0))
            if (c(0)(1) < c(1)(1))
              (c(0)(1) to c(1)(1)).map((c(0)(0), _))
            else
              (c(1)(1) to c(0)(1)).map((c(0)(0), _))
          else
            if (c(0)(0) < c(1)(0))
              (c(0)(0) to c(1)(0)).map((_, c(1)(1)))
            else
              (c(1)(0) to c(0)(0)).map((_, c(1)(1)))
        )
      ).flatten.toSet

  val (xmin, xmax) = (rock.minBy(_._1)._1, rock.maxBy(_._1)._1)

  def sandFall(fallen: Int, grain: (Int, Int), pile: Set[(Int, Int)]): Int =
    if (grain._1 < xmin || grain._1 > xmax)
      fallen
    else
      val (down, left, right) = ((grain._1, grain._2+1), (grain._1-1, grain._2+1), (grain._1+1, grain._2+1))

      // Make the grain fall
      if (!pile.contains(down))
        sandFall(fallen, down, pile)
      else if (!pile.contains(left))
        sandFall(fallen, left, pile)
      else if (!pile.contains(right))
        sandFall(fallen, right, pile)
      else
        sandFall(fallen+1, origin, pile+grain)

  sandFall(0, origin, rock)

def ex14B(reader: Source) =
  val origin = (500,0)

  val rock = reader.getLines.map(_.split(" -> ").toList
    .map(_.split(",").map(_.toInt).toList).map({case List(x,y) => (x,y)}))
    .flatMap(l =>
      l.zip(l.drop(1)).map(c =>
        if (c(0)(0) == c(1)(0))
          if (c(0)(1) < c(1)(1))
            (c(0)(1) to c(1)(1)).map((c(0)(0), _))
          else
            (c(1)(1) to c(0)(1)).map((c(0)(0), _))
        else
          if (c(0)(0) < c(1)(0))
            (c(0)(0) to c(1)(0)).map((_, c(1)(1)))
          else
            (c(1)(0) to c(0)(0)).map((_, c(1)(1)))
      )
    ).flatten.toSet

  val ymax = rock.maxBy(_._2)._2

  def sandFall(fallen: Int, grain: (Int, Int), pile: Set[(Int, Int)]): Int =
    val (down, left, right) = ((grain._1, grain._2+1), (grain._1-1, grain._2+1), (grain._1+1, grain._2+1))

    // Make the grain fall
    if (down._2 == ymax+2)
      sandFall(fallen+1, origin, pile+grain)
    else if (!pile.contains(down))
      sandFall(fallen, down, pile)
    else if (!pile.contains(left))
      sandFall(fallen, left, pile)
    else if (!pile.contains(right))
      sandFall(fallen, right, pile)
    else if (grain == origin)
      // Base case!!
      fallen+1
    else
      sandFall(fallen+1, origin, pile+grain)

  sandFall(0, origin, rock)

def ex12A(reader: Source) =
  // Create a height map of the area
  var heightMap = reader.getLines
    .map(_.toList.zipWithIndex)
    .zipWithIndex
    .flatMap((l: List[(Char, Int)], i: Int) => l.map((c: Char, j: Int) => ((j, i), c)))
    .toMap

  val start = heightMap.find(_._2 == 'S').get(0)
  val end = heightMap.find(_._2 == 'E').get(0)

  heightMap = heightMap ++ Map(start -> 'a', end -> 'z')

  def BFS(toVisit: List[(Int, Int)], visited: Map[(Int, Int), Int]): Map[(Int, Int), Int] = toVisit match
    case head :: tail =>
      // Check if the positions in each direction are visitable from the current positions
      val visitable2 = List(
        (head._1+1,head._2),
        (head._1-1,head._2),
        (head._1,head._2+1),
        (head._1,head._2-1)
      )
        // Accessible by height and exists on the map
        .filter(heightMap.contains(_))
        //.filter(heightMap(head)+1 <= heightMap(_))

      val visitable = visitable2.filter(heightMap(head).toInt + 1 >= heightMap(_).toInt)

      // Create new map entries when the coordinate doesnt exist or has worse distance
      val pathUpdates = visitable
        .filter(visited.getOrElse(_,Int.MaxValue)>visited(head)+1)
        .map((_,visited(head)+1))

      // Don't try to visit locations that have been visited
      BFS(tail ++ visitable.filter(!visited.contains(_)), visited ++ pathUpdates)

    case Nil => visited

  BFS(List(start),Map(start -> 0))(end)


def ex12B(reader: Source) =
  // Create a height map of the area
  var heightMap = reader.getLines
    .map(_.toList.zipWithIndex)
    .zipWithIndex
    .flatMap((l: List[(Char, Int)], i: Int) => l.map((c: Char, j: Int) => ((j, i), c)))
    .toMap

  val start = heightMap.filter(entry => (entry._2 == 'S') || (entry._2 == 'a'))
  val originalstart = heightMap.find(_._2 == 'S').get(0)
  val end = heightMap.find(_._2 == 'E').get(0)

  heightMap = heightMap ++ Map(originalstart -> 'a', end -> 'z')

  def BFS(toVisit: List[(Int, Int)], visited: Map[(Int, Int), Int]): Map[(Int, Int), Int] = toVisit match
    case head :: tail =>
      // Check if the positions in each direction are visitable from the current positions
      val visitable2 = List(
        (head._1+1,head._2),
        (head._1-1,head._2),
        (head._1,head._2+1),
        (head._1,head._2-1)
      )
        // Accessible by height and exists on the map
        .filter(heightMap.contains(_))
      //.filter(heightMap(head)+1 <= heightMap(_))

      val visitable = visitable2.filter(heightMap(head).toInt + 1 >= heightMap(_).toInt)

      // Create new map entries when the coordinate doesnt exist or has worse distance
      val pathUpdates = visitable
        .filter(visited.getOrElse(_,Int.MaxValue)>visited(head)+1)
        .map((_,visited(head)+1))

      // Don't try to visit locations that have been visited
      BFS(tail ++ visitable.filter(!visited.contains(_)), visited ++ pathUpdates)

    case Nil => visited

  println(start.toList.map(_._1))
  start.toList.map(_._1).map(c => BFS(List(c),Map(c -> 0)).getOrElse(end,Int.MaxValue)).toList.min


def ex10A(reader: Source) =
  (Seq(0) ++ reader.getLines
    .flatMap(l => if (l.take(4)=="noop") List(0) else List(0, l.split(" ")(1).toInt)))
    .grouped(20)
    .scanLeft(1)(_+_.sum)
    .zipWithIndex
    .filter((_, i) => i % 2 == 1)
    .take(6)
    .map((v, i) => 20 * i * v)
    .sum

def ex10B(reader: Source) =
  reader.getLines
    .flatMap(l => if (l.take(4)=="noop") List(0) else List(0, l.split(" ")(1).toInt))
    .scanLeft(1)(_+_)
    .zipWithIndex
    .map((v, i) => if (i % 40 + 1 >= v && v >= i % 40 - 1) "#" else ".")
    //.map((v, i) => (i % 40, v))
    .grouped(40)
    .map(_.mkString(""))
    .mkString("\n")

def ex9A(reader: Source) =
  case class Coordinates(head: (Int, Int), tail: (Int, Int), tailVisited: Set[(Int, Int)])
  reader.getLines
    .flatMap(s => List.fill(s.split(" ")(1).toInt)(s.split(" ")(0))) // Flatten into a list of single steps
    .foldLeft(Coordinates((0,0),(0,0),Set((0,0))))((C: Coordinates, s: String) =>
      val (hX, hY) = C.head
      val (tX, tY) = C.tail
      s match
        case "U" =>
          val headNew = (hX, hY+1)
          if (hY <= tY)
            // Tail doesn't move if not forced
            C.copy(head = headNew)
          else
            if (hX == tX)
              // Move up vertically
              val tailNew = (tX, tY+1)
              Coordinates(headNew, tailNew, C.tailVisited + tailNew)
            else
              // Move up diagonally
              val tailNew = (hX, tY+1)
              Coordinates(headNew, tailNew, C.tailVisited + tailNew)
        case "D" =>
          val headNew = (hX, hY - 1)
          if (hY >= tY)
            // Tail doesn't move if not forced
            C.copy(head = headNew)
          else
            if (hX == tX)
              // Move down vertically
              val tailNew = (tX, tY - 1)
              Coordinates(headNew, tailNew, C.tailVisited + tailNew)
            else
              // Move down diagonally
              val tailNew = (hX, tY - 1)
              Coordinates(headNew, tailNew, C.tailVisited + tailNew)
        case "L" =>
          val headNew = (hX - 1, hY)
          if (hX >= tX)
            // Tail doesn't move if not forced
            C.copy(head = headNew)
          else
            if (hY == tY)
              // Move left horizontally
              val tailNew = (tX - 1, tY)
              Coordinates(headNew, tailNew, C.tailVisited + tailNew)
            else
              // Move left diagonally
              val tailNew = (tX - 1, hY)
              Coordinates(headNew, tailNew, C.tailVisited + tailNew)
        case "R" =>
           val headNew = (hX + 1, hY)
           if (hX <= tX)
              // Tail doesn't move if not forced
              C.copy(head = headNew)
           else
             if (hY == tY)
               // Move right horizontally
               val tailNew = (tX + 1, tY)
               Coordinates(headNew, tailNew, C.tailVisited + tailNew)
             else
               // Move right diagonally
               val tailNew = (tX + 1, hY)
               Coordinates(headNew, tailNew, C.tailVisited + tailNew)
    ).tailVisited.size

def ex9B(reader: Source) =
    case class Coordinates(head: (Int, Int), tail: List[(Int, Int)], tailVisited: Set[(Int, Int)])
    reader.getLines
      .flatMap(s => List.fill(s.split(" ")(1).toInt)(s.split(" ")(0))) // Flatten into a list of single steps
      .foldLeft(Coordinates((0, 0), List.fill(9)(0,0), Set((0, 0))))((C: Coordinates, s: String) =>
        val (hX, hY) = C.head
        val headNew = s match
          case "U" => (hX, hY + 1)
          case "D" => (hX, hY - 1)
          case "L" => (hX - 1, hY)
          case "R" => (hX + 1, hY)

        val tailNew = C.tail.zip(headNew :: C.tail).map(c =>
          val ((tX,tY),(hX,hY)) = c
          val (horizontal, vertical) = ((tX-hX).abs == 2, (tY-hY).abs == 2)
          (
            // If not compelled to move horizontally by head
            if (!horizontal)
              // If diagonal movement
              if (vertical && tX != hX)
                hX
              // If no horizontal movement
              else
                tX
            else
              // Compelled to move by head
              tX + (hX-tX) / 2
            ,
            // If not compelled to move vertically by head
            if (!vertical)
            // If diagonal movement
              if (horizontal && tY != hY)
                hY
              // If no horizontal movement
              else
                tY
            else
            // Compelled to move by head
              tY + (hY - tY) / 2
          )
        )

        Coordinates(headNew, tailNew, C.tailVisited ++ tailNew.takeRight(1))

      ).tailVisited.size

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