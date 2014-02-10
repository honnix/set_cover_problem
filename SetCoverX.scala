object SetCoverX extends App {
  def f(cover: Seq[Set[Int]]) = cover.foldLeft(Set[Int]())(_ ++ _)

  val Target = Set(1, 2, 3, 4, 5, 6, 7)

  val S = Seq((14, Set(1, 2, 3)),
              (9, Set(1, 2)),
              (3, Set(3)),
              (8, Set(4, 5, 6)),
              (1, Set(8, 9, 10)),
              (1, Set(1, 9, 10)),
              (10, Set(4, 5, 6, 7)),
              (7, Set(5, 6, 7)))

  val FS = Target

  def computeCover(cover: Seq[Set[Int]], price: Int): (Int, Seq[Set[Int]]) = {
    // for real resolution problem, "contains", "f" and "--" should be revised
    def pricePerElement(selection: (Int, Set[Int])) = {
      def effectiveSize(set: Set[Int]) = set.foldLeft(0)((x, y) => x + (if (FS.contains(y)) 1 else 0))

      val size = effectiveSize(f(cover :+ selection._2) -- f(cover))
      if (size == 0) Double.MaxValue
      else selection._1.toDouble / size
    }

    if ((FS -- f(cover)).nonEmpty) {
      val min = S.map { x =>
        (pricePerElement(x), x._1, x._2)
      }.minBy(_._1)
      computeCover(cover :+ min._3, price + min._2)
    } else (price, cover)
  }

  val cover = computeCover(Seq[Set[Int]](), 0)

  println(cover)
}
