package com.madsen.gameproto.octree

import com.madsen.gameproto.octree.Octree._

object Octree {
  type Point = (Long, Long, Long)


  def apply[T](): Octree[T] = new Empty[T]


  private def parent(centre: Long, radius: Long): (Long, Long) = {
    val (x, r) = (centre, radius)
    val y: Long = x + r - 2 * r * (((x - r) / (2 * r)) % 2)

    (y, 2 * r)
  }

  case class Node[T](
    centre: Point, // min centre point is (1,1,1) so we don't have to deal with fractions.
    radius: Long, // min radius should be 1 so we don't have to deal with fractions.
    children: Vector[Octree[T]]
  ) extends Octree[T] {

    def add(value: T, centre: Point): Octree[T] = ???


    def findWithinDistanceOf(value: T, radius: Long): Iterable[T] = ???
  }

  case class Leaf[T](value: T, centre: Point) extends Octree[T] {

    def add(value: T, centre: Point): Octree[T] = {
      if (this.centre == centre) Leaf(value, centre)
      else {
        ??? // TODO: Leaves can only have odd points (to avoid halves). Add node and recurse
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

