object SetCover extends App {
  def f(cover: Seq[Set[Int]]) = cover.foldLeft(Set[Int]())(_ ++ _)

  val S = Seq((14, Set(1, 2, 3)),
              (9, Set(1, 2)),
              (3, Set(3)),
              (8, Set(4, 5, 6)),
              (10, Set(4, 5, 6, 7)),
              (7, Set(5, 6, 7)))
  val FS = f(S.map(_._2))

  def computeCover(cover: Seq[Set[Int]], price: Int): (Int, Seq[Set[Int]]) = {
    def pricePerElement(selection: (Int, Set[Int])) = {
      val diff = f(cover :+ selection._2) -- f(cover)
      if (diff.size == 0) Double.MaxValue
      else selection._1.toDouble / diff.size
    }

    if (f(cover) != FS) {
      val min = S.map { x =>
        (pricePerElement(x), x._1, x._2)
      }.minBy(_._1)
      computeCover(cover :+ min._3, price + min._2)
    } else (price, cover)
  }

  var cover = computeCover(Seq[Set[Int]](), 0)

  println(cover)
}
