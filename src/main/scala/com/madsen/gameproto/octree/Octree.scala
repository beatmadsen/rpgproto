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


  // TODO: Find existing node that leaf may belong under if one exists
  private def childFor[T](point: Point, candidates: Vector[Octree[T]]): Option[Octree[T]] = ???


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


    // TODO: By 'nature' only 8 children (as defined by points) should fit in each node

    // TODO: when we add upwards we let the new node decide what to return, but we should always return the root of the tree
    def add(value: T, centre: Point): Octree[T] = {

      val belongsUnderThisNode: Boolean = ???

      def createSubtree(value: T, centre: Point): Octree[T] = radius match {
        case 2 ⇒ Leaf(value, centre)
        case n if n > 2 ⇒
          val p: Point = ??? // TODO: Where should the node be added? - 8 potential locations
          Node(p, n / 2, Vector.empty).add(value, centre)
      }

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
	
	
	private def rise(value: T, centre: Point): Octree[T] = {
		// we know from the radius of this guy how many steps we are from bottom.
		???
	}


    def findWithinDistanceOf(value: T, radius: Long): Iterable[T] = ???
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

