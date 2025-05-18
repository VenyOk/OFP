% Лабораторная работа № 4 «Case-классы и сопоставление с образцом в Scala»
% 23 апреля 2025 г.
% Вениамин Шемякин, ИУ9-62Б

# Цель работы
Целью данной работы является приобретение навыков разработки case-классов на языке Scala для 
представления абстрактных синтаксических деревьев.
# Индивидуальный вариант
Абстрактный синтаксис арифметических выражений:

```Expr → Expr + Expr | Expr - Expr | Expr * Expr | Expr / Expr | VARNAME```

Написать функцию ratioPolynomns : Expr => Expr, которая преобразует выражение в полином 
(если в нём нет операции деления) или в отношение двух полиномов (если операция деления есть).

# Реализация

```scala
sealed trait Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Mul(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Var(name: String) extends Expr

object ExprSyntax {
  implicit final class InfixOps(private val lhs: Expr) extends AnyVal {
    def +(rhs: Expr): Expr = Add(lhs, rhs)
    def -(rhs: Expr): Expr = Sub(lhs, rhs)
    def *(rhs: Expr): Expr = Mul(lhs, rhs)
    def /(rhs: Expr): Expr = Div(lhs, rhs)
  }
}

object Main extends App {
  import ExprSyntax._

  private val one: Expr = Var("1")
  private type Rational = (Expr, Expr)

  private def times(a: Expr, b: Expr): Expr = (a, b) match {
    case (_, x) if x == one => a
    case (x, _) if x == one => b
    case (x, y)             => Mul(x, y)
  }

  private def combineAddSub(l: Expr, r: Expr, ctor: (Expr, Expr) => Expr): Rational = {
    val (n1, d1) = normalize(l)
    val (n2, d2) = normalize(r)
    if (d1 == d2) (ctor(n1, n2), d1)
    else (ctor(times(n1, d2), times(n2, d1)), times(d1, d2))
  }

  private def normalize(expr: Expr): Rational = expr match {
    case v: Var => (v, one)
    case Add(l, r) => combineAddSub(l, r, Add.apply)
    case Sub(l, r) => combineAddSub(l, r, Sub.apply)
    case Mul(l, r) =>
      val (n1, d1) = normalize(l)
      val (n2, d2) = normalize(r)
      (times(n1, n2), times(d1, d2))
    case Div(l, r) =>
      val (n1, d1) = normalize(l)
      val (n2, d2) = normalize(r)
      (times(n1, d2), times(d1, n2))
  }

  def ratioPolynoms(expr: Expr): Expr = {
    val (num, den) = normalize(expr)
    if (den == one) num else Div(num, den)
  }

  val expr1 = Var("x") + Var("y")
  val expr2 = (Var("x") + Var("y")) * (Var("z") - Var("x"))
  val expr3 = (Var("x") + Var("y")) / (Var("z") * Var("x"))
  val expr4 = (Var("a") / Var("b") + Var("c")) / Var("d")
  val expr5 = Var("a") / Var("b") + Var("c") / Var("d")
  val expr6 = (Var("p") / Var("q")) * Var("1")

  List(expr1, expr2, expr3, expr4, expr5, expr6).zipWithIndex.foreach {
    case (e, i) => println(s"${i + 1}: ${ratioPolynoms(e)}")
  }
}
```

# Тестирование

```
1: Add(Var(x),Var(y))
2: Mul(Add(Var(x),Var(y)),Sub(Var(z),Var(x)))
3: Div(Add(Var(x),Var(y)),Mul(Var(z),Var(x)))
4: Div(Add(Var(a),Mul(Var(c),Var(b))),Mul(Var(b),Var(d)))
5: Div(Add(Mul(Var(a),Var(d)),Mul(Var(c),Var(b))),Mul(Var(b),Var(d)))
6: Div(Var(p),Var(q))
```

# Вывод
В ходе данной лабораторной работы я научился работать с case-классами для представления абстрактных
деревьев, реализовав программу для арифметических выражений.