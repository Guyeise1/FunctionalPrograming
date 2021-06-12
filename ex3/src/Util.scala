import scala.reflect.ClassTag

object Util {
  def entropy(xs: Array[Double]): Double = {
    -xs.groupBy(identity)
      .map(_._2.length.toDouble / xs.length)
      .map(p => p * math.log(p) / math.log(2))
      .sum
  }

  def remove [A: ClassTag] (xs: Array[A], index: Int): Array[A] = {
    xs.zipWithIndex.filter(_._2 != index).map(_._1)
  }

  def anomaly(xs: Array[Double], index: Int): Double = {
    entropy(xs) - entropy(remove(xs, index))
  }
  def maxAnomaly(xs: Array[Double]): (Double, Int) = {
    xs.indices.map(anomaly(xs, _)).zipWithIndex.maxBy(_._1)
  }

}
