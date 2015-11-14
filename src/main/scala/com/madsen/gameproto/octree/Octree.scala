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
    children: Either[Map[Point, Leaf[T]], Map[Point, Node[T]]]
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
        addChild(value, createSubtreeChain(centre))
      } else {
        val (parentCentre, parentRadius) = parent(centre, radius)
        val children: Map[Point, Node[T]] = Map(this.centre → this)

        val node: Node[T] = Node(parentCentre, parentRadius, Right(children))

        node.add(value, centre)
      }
    }


    def findWithinDistanceOf(value: T, radius: Long): Iterable[T] = ???


    def addChild(value: T, chain: List[Point]): Node[T] = {

      val centre: Point = chain.head

      def locateOrCreateChildNode(nodes: Map[Point, Node[T]]) = {
        assert(chain.size > 1)

        nodes.getOrElse(centre, {
          val newChildren: Either[Map[Point, Leaf[T]], Map[Point, Node[T]]] =
            if (chain.size == 2) Left(Map.empty)
            else Right(Map.empty)

          Node(centre, this.radius / 2, newChildren)
        })
      }


      def locateOrCreateChildLeaf(nodes: Map[Point, Leaf[T]]) = {
        assert(chain.size == 1)
        nodes.getOrElse(centre, Leaf(value, centre))
      }


      val c1 = children
        .right
        .map { nodes ⇒
          val updatedChild: Node[T] = locateOrCreateChildNode(nodes).addChild(value, chain.tail)
          nodes + (centre → updatedChild)
        }
        .left
        .map { leaves ⇒
          val leaf = locateOrCreateChildLeaf(leaves)
          leaves + (centre → leaf)
        }

      copy(children = c1)
    }


    private def createSubtreeChain(centre: Point): List[Point] = ???


    //    private def createSubtree(value: T, centre: Point): Octree[T] = {
    //      // start with the leaf
    //      val leaf = Leaf(value, centre)
    //
    //      // now get parent for leaf and keep adding until parent == this
    //      val nextParent: ((Point, Long)) ⇒ (Point, Long) = (parent _).tupled
    //
    //      @tailrec
    //      def helper(current: (Point, Long), acc: Octree[T]): Octree[T] = nextParent(current) match {
    //        case (this.centre, this.radius) ⇒ acc // next parent is this node
    //        case next @ (nextCentre, nextRadius) ⇒ // otherwise create missing step
    //          val node = Node(nextCentre, nextRadius, Map(acc.centre → acc))
    //          helper(next, node)
    //      }
    //
    //      helper((centre, 1L), leaf)
    //    }
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
        val children: Map[Point, Leaf[T]] = Map(this.centre → this)

        val node: Node[T] = Node(parentCentre, parentRadius, Left(children))

        node.add(value, centre)
      }
    }


    def findWithinDistanceOf(value: T, radius: Long): Iterable[T] = ???
  }


  class Empty[T] extends Octree[T] {
    // TODO: This one doesn't make sense

    def add(value: T, centre: Point): Octree[T] = Leaf(value, centre)


    def findWithinDistanceOf(value: T, radius: Long): Iterable[T] = Iterable.empty
  }

}

trait Octree[T] {

  def add(value: T, centre: Point): Octree[T]

  def findWithinDistanceOf(value: T, radius: Long): Iterable[T]
}

