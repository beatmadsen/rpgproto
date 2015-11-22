package com.madsen.gameproto.octree

import com.madsen.gameproto.octree.Octree._

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange

object Octree {

  type Point = (Long, Long, Long)

  val PowersOfTwo: Stream[Long] = Stream.continually(2L).scanLeft(1L) { (a, b) ⇒ a * b }


  def apply[T](): Octree[T] = new Empty[T]


  def empty[T]: Octree[T] = new Empty[T]


  def isPowerOfTwo(candidate: Long): Boolean = {

    val rest: Stream[Long] = PowersOfTwo dropWhile (_ < candidate)
    rest.head == candidate
  }


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


  case class Node[+B](
    centre: Point,
    radius: Long,
    children: Either[Map[Point, Leaf[B]], Map[Point, Node[B]]]
  ) extends Octree[B] {

    require(isPowerOfTwo(radius))
    require {
      val (cx, cy, cz) = centre
      List(cx, cy, cz) forall { i ⇒ i >= 1 && (1 + i / radius) % 2 == 0 }
    }


    def +[B1 >: B](kv: (Point, B1)): Octree[B1] = kv match {
      case (point, value) ⇒
        if (belongsUnderThisNode(point)) addChild(value, createSubtreeChain(point))
        else addToNewParent(value, point)
    }


    def get(key: Point): Option[B] = {

      val path: List[Point] = findTraversalPath(key)

      get(path)
    }


    def iterator: Iterator[(Point, B)] = ??? // TODO: depth first


    def -(key: Point): Octree[B] = ??? // TODO: Kinda like get - find right node and copy without this leaf.


    def findWithinDistanceOf[B1 >: B](value: B1, radius: Long): Iterable[B1] = ???


    private def findTraversalPath(point: Point): List[Point] = {

      @tailrec
      def helper(current: (Point, Long), acc: List[Point]): List[Point] = current match {
        case (p, r) if r == this.radius ⇒ p :: acc
        case (p, r) ⇒ helper(parent(p, r), p :: acc)
      }

      val list = helper((point, 1), Nil)

      list match {
        case x :: xs if x != this.centre ⇒ Nil
        case xs ⇒ xs
      }
    }


    private def get(traversalPath: List[Point]): Option[B] = traversalPath match {
      case Nil ⇒ None
      case p :: ps ⇒
        children
          .right
          .map { nodes ⇒ (nodes get p) flatMap { node ⇒ node get ps } }
          .left
          .map { leaves ⇒ (leaves get p) map { leaf ⇒ leaf.value } }
          .merge
    }


    private def addToNewParent[B1 >: B](value: B1, centre: Point): Octree[B1] = {

      val (parentCentre, parentRadius) = parent(centre, radius)
      val children: Map[Point, Node[B1]] = Map(this.centre → this)
      val node: Node[B1] = Node(parentCentre, parentRadius, Right(children))

      node + (centre → value)
    }


    private def belongsUnderThisNode(centre: Point): Boolean = {

      val (x0, y0, z0) = this.centre
      val (x1, y1, z1) = centre

      def leafScalars(centreScalar: Long): NumericRange[Long] = {

        val upperBound = centreScalar + radius / 2 - 1
        val lowerBound = centreScalar - radius / 2 + 1

        lowerBound.to(upperBound, 2L)
      }

      (leafScalars(x0) contains x1) &&
        (leafScalars(y0) contains y1) &&
        (leafScalars(z0) contains z1)
    }


    private def addChild[B1 >: B](value: B1, chain: List[Point]): Node[B1] = {

      val centre: Point = chain.head

      def locateOrCreateChildNode(nodes: Map[Point, Node[B1]]) = nodes.getOrElse(centre, {
        val newChildren: Either[Map[Point, Leaf[B1]], Map[Point, Node[B1]]] =
          if (chain.size == 2) Left(Map.empty)
          else Right(Map.empty)

        Node(centre, this.radius / 2, newChildren)
      })

      def locateOrCreateChildLeaf(nodes: Map[Point, Leaf[B1]]) = nodes.getOrElse(centre, Leaf(value, centre))

      def updateNodes(children: Map[Point, Node[B1]]): Map[Point, Node[B1]] = {

        assert(chain.size > 1)
        val updatedChild: Node[B1] = locateOrCreateChildNode(children).addChild(value, chain.tail)
        children + (centre → updatedChild)
      }

      def updateLeaves(children: Map[Point, Leaf[B1]]): Map[Point, Leaf[B1]] = {

        assert(chain.size == 1)
        val updatedChild: Leaf[B1] = locateOrCreateChildLeaf(children)
        children + (centre → updatedChild)
      }

      copy(children = children.right.map(updateNodes).left.map(updateLeaves))
    }


    private def createSubtreeChain(centre: Point): List[Point] = {

      @tailrec
      def helper(current: (Point, Long), acc: List[Point]): List[Point] = current match {
        case (this.centre, this.radius) ⇒ acc // next parent is this node
        case (p, r) ⇒ helper(parent(p, r), p :: acc)
      }

      helper((centre, 1L), Nil)
    }
  }


  /**
    * Leaves have radius = 1 and must be placed on odd coordinates
    * @param value
    * @param centre
    * @tparam B
    */
  case class Leaf[+B](value: B, centre: Point) extends Octree[B] {

    require {
      val (cx, cy, cz) = centre
      List(cx, cy, cz) forall { i ⇒ i >= 1 && i % 2 == 1 }
    }


    def findWithinDistanceOf[B1 >: B](value: B1, radius: Long): Iterable[B1] = ???


    def +[B1 >: B](kv: (Point, B1)): Octree[B1] = {

      val (centre, value) = kv

      if (this.centre == centre) Leaf(value, centre)
      else {
        val (parentCentre, parentRadius) = parent(centre, 1L)
        val children: Map[Point, Leaf[B1]] = Map(this.centre → this)

        val node: Node[B1] = Node(parentCentre, parentRadius, Left(children))

        node + (centre → value)
      }
    }


    def iterator: Iterator[(Point, B)] = Iterator((centre, value))


    def -(key: Point): Octree[B] = get(key) map { _ ⇒ Octree.empty } getOrElse this


    def get(key: Point): Option[B] = key match {
      case c if c == this.centre ⇒ Some(value)
      case _ ⇒ None
    }
  }


  class Empty[B] extends Octree[B] {

    def findWithinDistanceOf[B1 >: B](value: B1, radius: Long): Iterable[B1] = Iterable.empty


    def +[B1 >: B](kv: (Point, B1)): Octree[B1] = kv match {
      case (key, value) ⇒ Leaf(value, key)
    }


    def iterator: Iterator[(Point, B)] = Iterator.empty


    def get(key: Point): Option[B] = None


    def -(key: Point): Octree[B] = this
  }


}


trait Octree[+B] extends Map[Point, B] {


  def findWithinDistanceOf[B1 >: B](value: B1, radius: Long): Iterable[B1]
}

