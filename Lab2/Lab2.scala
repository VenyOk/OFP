class Polynomial(coefficients: Map[Int, Int]) {
  var coef: Map[Int, Int] = coefficients

  def this(coeffs: (Int, Int)*) = this(coeffs.toMap)

  def +(other: Polynomial): Polynomial = {
    new Polynomial((coef.keySet ++ other.coef.keySet).map {
      degree => degree -> (coef.getOrElse(degree, 0) + other.coef.getOrElse(degree, 0))
    }.toMap)
  }

  def *(other: Polynomial): Polynomial = {
    new Polynomial((for {
      (deg1, coeff1) <- coef
      (deg2, coeff2) <- other.coef
    } yield (deg1 + deg2) -> (coeff1 * coeff2)).groupBy(_._1).map {
      case (deg, coeffs) => deg -> coeffs.map(_._2).sum
    })
  }

  def unary_! : Polynomial = {
    new Polynomial(coef.map {
      case (degree, coeff) => (degree - 1) -> (coeff * degree)
    }.filter(_._1 >= 0))
  }

  override def toString: String = {
    coef.toList.sortBy(-_._1).map {
      case (degree, coeff) => s"${coeff}x^$degree"
    }.mkString(" + ")
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val p1 = new Polynomial(3 -> 2, 2 -> 1, 1 -> -1, 0 -> 3)
    val p2 = new Polynomial(2 -> 4, 1 -> 3)
    println(s"p1 = $p1")
    println(s"p2 = $p2")

    val sum = p1 + p2
    println(s"sum: $sum")

    val product = p1 * p2
    println(s"product: $product")

    val derivative = !p1
    println(s"derivative: $derivative")
  }
}
