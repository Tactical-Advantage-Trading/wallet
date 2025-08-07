package immortan.utils


object Statistics {
  def meanBy[T](items: Seq[T] = Nil)(extractor: T => Long): Double = {
    val mean = items.foldLeft(0D) { case (total, item) => extractor(item).toDouble + total }
    mean / items.size
  }

  def medianBy[T](items: Seq[T] = Nil, skew: Double = 0.5D)(extractor: T => Long): Long = {
    extractor(items.sortBy(extractor).drop(n = (items.size * skew).toInt).head)
  }

  def stdDevBy[T](items: Seq[T] = Nil, mean: Double)(extractor: T => Long): Double = {
    val variance = items.foldLeft(0D) { case (total, item) => math.pow(extractor(item) - mean, 2) + total }
    math.sqrt(variance / items.size.toDouble)
  }
}