val is_sorted: ((Int, Int) => Boolean) => (List[Int] => Boolean) = {
  case comparison => {
    case Nil => true
    case _ :: Nil => true
    case x :: y :: tail => comparison(x, y) && is_sorted(comparison)(y :: tail)
  }
}

object Main extends App {
  val asc = (x: Int, y: Int) => x <= y
  val desc = (x: Int, y: Int) => x >= y

  val list1 = List(1, 2, 3, 4)
  val list2 = List(4, 3, 2, 1)
  val list3 = List(1, 3, 2, 4)
  val list4 = List(1)
  val list5 = List()

  println(s"${is_sorted(asc)(list1)}")
  println(s"${is_sorted(asc)(list2)}")
  println(s"${is_sorted(asc)(list3)}")
  println(s"${is_sorted(asc)(list4)}")
  println(s"${is_sorted(asc)(list5)}")

  println("---------------------")

  println(s"${is_sorted(desc)(list1)}")
  println(s"${is_sorted(desc)(list2)}")
  println(s"${is_sorted(desc)(list3)}")
  println(s"${is_sorted(desc)(list4)}")
  println(s"${is_sorted(desc)(list5)}")

}
