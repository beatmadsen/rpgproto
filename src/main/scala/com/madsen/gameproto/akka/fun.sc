

object Math {

  trait CanDance[T] {

    def goDance(t: T): T
  }

  object CanDance {

    implicit object CanDanceInt extends CanDance[Int] {
      override def goDance(t: Int): Int = t + 42
    }

    implicit object CanDanceString extends CanDance[String] {
      override def goDance(t: String): String = t + " - Dance"
    }

  }

}


object Statistics {

  import Math.CanDance

  def worry[T: CanDance](x: T) = {

    implicitly[CanDance[T]].goDance(x)
  }
}

object Pimp {

  import Math.CanDance

  implicit object CanDanceDouble extends CanDance[Double] {
    override def goDance(t: Double): Double = t * 1.2
  }

}


object Lama {

  import Pimp._
  import Statistics.worry

  def something = worry(42.3)
}


Lama.something