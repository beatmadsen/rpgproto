package com.madsen.gameproto.octree

import com.madsen.gameproto.octree.Octree._

object Octree {
  type Point = (Long, Long, Long)

  val PowersOfTwo: Stream[Long] = Stream.continually(2L).scanLeft(1L) { (a, b) ⇒ a * b }


  def isPowerOfTwo(candidate: Long): Boolean = {

    val rest: Stream[Long] = PowersOfTwo dropWhile (_ < candidate)
    rest.head == candidate
  }


  isPowerOfTwo(32)


  def apply[T](): Octree[T] = new Empty[T]


  private def parent(point: Point, radius: Long): (Point, Long) = {
    val (x, y, z) = point
    val newPoint: Point = List(x, y, z) map { scalar ⇒ parentScalar(scalar, radius) } match {
      case x1 :: y1 :: z1 :: _ ⇒ (x1, y1, z1)
    }

    (newPoint, radius * 2)
  }


  private def parentScalar(centre: Long, radius: Long): Long = {
    val (x, r) = (centre, radius)
    x + r - 2 * r * (((x - r) / (2 * r)) % 2)
  }

  case class Node[T](
    centre: Point,
    radius: Long,
    children: Vector[Octree[T]]
  ) extends Octree[T] {
    require {
      val (cx, cy, cz) = centre
      List(cx, cy, cz) forall { i ⇒ i == 1 || (i > 1 && i % 2 == 0) }
    }
    require(isPowerOfTwo(radius))


    // TODO: By 'nature' only 8 children should fit in each node

    // TODO: when we add upwards we let the new node decide what to return, but we should always return the root of the tree
    def add(value: T, centre: Point): Octree[T] = ???


    def findWithinDistanceOf(value: T, radius: Long): Iterable[T] = ???
  }

  case class Leaf[T](value: T, centre: Point) extends Octree[T] {

    def add(value: T, centre: Point): Octree[T] = {
      if (this.centre == centre) Leaf(value, centre)
      else {
        val (parentCentre, parentRadius) = parent(centre, 1L)
        val children: Vector[Octree[T]] = Vector(this)

        val node: Node[T] = Node(parentCentre, parentRadius, children)

        node.add(value, centre)
      }
    }


    def findWithinDistanceOf(value: T, radius: Long): Iterable[T] = ???
  }

  class Empty[T] extends Octree[T] {

    def add(value: T, centre: Point): Octree[T] = Leaf(value, centre)


    def findWithinDistanceOf(value: T, radius: Long): Iterable[T] = Iterable.empty
  }

}

trait Octree[T] {
  def add(value: T, centre: Point): Octree[T]

  def findWithinDistanceOf(value: T, radius: Long): Iterable[T]
}

