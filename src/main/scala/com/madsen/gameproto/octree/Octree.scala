package com.madsen.gameproto.octree

import com.madsen.gameproto.octree.Octree._

import scala.annotation.tailrec

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
    require(isPowerOfTwo(radius))
    require {
      val (cx, cy, cz) = centre
      List(cx, cy, cz) forall { i ⇒ i >= 1 && (1 + i / radius) % 2 == 0 }
    }

    def add(value: T, centre: Point): Octree[T] = {

      // TODO: By 'nature' only 8 children (as defined by points) should fit in each node
      val belongsUnderThisNode: Boolean = ???

      if (belongsUnderThisNode) {
        val next: Octree[T] = childFor(centre, children) map { subtree ⇒
          subtree.add(value, centre)
        } getOrElse createSubtree(value, centre)

        val c: Vector[Octree[T]] = (children filterNot { tree ⇒ tree.centre == next.centre }) :+ next
        copy(children = c)
      } else {
        val (parentCentre, parentRadius) = parent(centre, radius)
        val children: Vector[Octree[T]] = Vector(this)

        val node: Node[T] = Node(parentCentre, parentRadius, children)

        node.add(value, centre)
      }
    }


    def findWithinDistanceOf(value: T, radius: Long): Iterable[T] = ???


    // TODO: Find existing node that leaf may belong under if one exists
    private def childFor(point: Point, candidates: Vector[Octree[T]]): Option[Octree[T]] = ???


    private def createSubtree(value: T, centre: Point): Octree[T] = {
      // start with the leaf
      val leaf = Leaf(value, centre)

      // now get parent for leaf and keep adding until parent == this
      val nextParent: ((Point, Long)) ⇒ (Point, Long) = (parent _).tupled

      @tailrec
      def helper(current: (Point, Long), acc: Octree[T]): Octree[T] = nextParent(current) match {
        case (this.centre, this.radius) ⇒ acc // next parent is this node
        case next @ (nextCentre, nextRadius) ⇒ // otherwise create missing step
          val node = Node(nextCentre, nextRadius, Vector(acc))
          helper(next, node)
      }

      helper((centre, 1L), leaf)
    }
  }

  /**
    * Leaves have radius = 1 and must be placed on odd coordinates
    * @param value
    * @param centre
    * @tparam T
    */
  case class Leaf[T](value: T, centre: Point) extends Octree[T] {

    require {
      val (cx, cy, cz) = centre
      List(cx, cy, cz) forall { i ⇒ i >= 1 && i % 2 == 1 }
    }


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


    def centre: (Long, Long, Long) = ???
  }

}

trait Octree[T] {
  def centre: Point

  def add(value: T, centre: Point): Octree[T]

  def findWithinDistanceOf(value: T, radius: Long): Iterable[T]
}

