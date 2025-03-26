trait Additive[T] {
  def plus(x: T, y: T): T
}

object Additive {
  implicit def numericAdditive[T](implicit num: Numeric[T]): Additive[T] =
    new Additive[T] {
      def plus(x: T, y: T): T = num.plus(x, y)
    }
  implicit val stringAdditive: Additive[String] =
    new Additive[String] {
      def plus(x: String, y: String): String = x + y
    }
}

trait Formula[T] {
  def eval(env: Map[String, T]): T
}

class Constant[T](val value: T) extends Formula[T] {
  def eval(env: Map[String, T]): T = value
}

class Variable[T](val name: String) extends Formula[T] {
  def eval(env: Map[String, T]): T =
    env.getOrElse(name, throw new NoSuchElementException(s"Переменная '$name' не определена"))
}

class Add[T](val left: Formula[T], val right: Formula[T])(implicit val additive: Additive[T])
    extends Formula[T] {
  def eval(env: Map[String, T]): T =
    additive.plus(left.eval(env), right.eval(env))
}

class Subtract[T](val left: Formula[T], val right: Formula[T])(implicit val num: Numeric[T])
    extends Formula[T] {
  def eval(env: Map[String, T]): T =
    num.minus(left.eval(env), right.eval(env))
}

class Multiply[T](val left: Formula[T], val right: Formula[T])(implicit val num: Numeric[T])
    extends Formula[T] {
  def eval(env: Map[String, T]): T =
    num.times(left.eval(env), right.eval(env))
}

class Divide[T](val left: Formula[T], val right: Formula[T])(implicit val frac: Fractional[T])
    extends Formula[T] {
  def eval(env: Map[String, T]): T =
    frac.div(left.eval(env), right.eval(env))
}

object FormulaOps {
  implicit class AddOps[T](val left: Formula[T]) extends AnyVal {
    def +(right: Formula[T])(implicit additive: Additive[T]): Formula[T] =
      new Add(left, right)
  }
  
  implicit class NumericOps[T](val left: Formula[T]) extends AnyVal {
    def -(right: Formula[T])(implicit num: Numeric[T]): Formula[T] =
      new Subtract(left, right)
    def *(right: Formula[T])(implicit num: Numeric[T]): Formula[T] =
      new Multiply(left, right)
    def /(right: Formula[T])(implicit frac: Fractional[T]): Formula[T] =
      new Divide(left, right)
  }
}

object Formula {
  def variable[T](name: String): Formula[T] = new Variable[T](name)
  def constant[T](value: T): Formula[T] = new Constant[T](value)
}

object Example extends App {
  import FormulaOps._
  val numFormula: Formula[Int] =
    (Formula.variable[Int]("x") + Formula.constant(10)) * Formula.constant(2)
  val envNum = Map("x" -> 5)
  println(s"Формула 1: ${numFormula.eval(envNum)}")
  val strFormula: Formula[String] =
    Formula.variable[String]("s") + Formula.constant[String](" world")
  val envStr = Map("s" -> "Hello")
  println(s"Формула 2: ${strFormula.eval(envStr)}")
}
