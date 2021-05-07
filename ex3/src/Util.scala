import scala.annotation.tailrec

object Util {


  // max
  def max[A](lst: List[A], comparator: (A, A) => Int): A = {
    @tailrec
    def loop(lst: List[A], m: A, comparator: (A, A) => Int): A = {
      if (lst.isEmpty) {
        m
      } else {
        if (comparator(m, lst.head) > 0) {
          loop(lst.tail, m, comparator)
        } else {
          loop(lst.tail, lst.head, comparator)
        }
      }
    }

    loop(lst, lst.head, comparator)
  }

  // map
  def map[A, B, C](lst: List[A], f: A => B, g: B => C): List[C] = {
    lst.map(a => g(f(a)))
  }

  // isSorted
  def isSorted[A](lst: List[A], comparator: (A, A) => Boolean): Boolean = {
    if (lst.isEmpty || lst.length == 1) {
      true
    } else {
      val (l1, l2) = lst.splitAt(lst.length / 2)
      val (r1, r2) = (isSorted(l1, comparator), isSorted(l2, comparator))
      r1 && r2 && comparator(l1.last, l2.head)
    }
  }

  // probs
  def probs(xs: Array[Double]): Array[Double] = {
    val p = xs.groupBy(x => x).map(e => (e._1, e._2.length / xs.length.toDouble))
    xs.map(x => p(x))
  }

  // entropy
  def entropy(arr: Array[Double]): Double = {
    math abs probs(arr).map(p => -p * Math.log10(p) / Math.log10(2)).sum
  }

  def avarage(xs: Array[Double]): Double = {
    xs.sum / xs.length
  }

  // mu
  def mu(arr: Array[Double]): Double = {
    arr.zip(probs(arr)).distinct.map(x => x._1 * x._2).sum
  }

  // variance
  def variance(arr: Array[Double]): Double = {
    val m = avarage(arr)
    arr.map(x => math.pow(x - m, 2)).sum / arr.length
  }

  // zscore
  def zscore(arr: Array[Double], x: Double): Double = {
    (x - mu(arr)) / math.sqrt(variance(arr))
  }

  // cov
  def cov(x: Array[Double], y: Array[Double]): Double = {
    val mx = avarage(x)
    val my = avarage(y)
    (x zip y).map(e => (e._1 - mx) * (e._2 - my)).sum / x.length
  }

  // pearson
  def pearson(x: Array[Double], y: Array[Double]): Double = {
    cov(x, y) / math.sqrt(variance(x) * variance(y))
  }

  def standardDeviation(arr: Array[Double]): Double = math.sqrt(variance(arr))

  private def firstElementCorrelation(elements: Map[String, Vector[Double]]): Array[(String, Double)] = {
    elements.tail.map(e => (e._1, Util.pearson(elements.head._2.toArray, e._2.toArray)
    )).toArray
  }


  def correlation(elements: Map[String, Vector[Double]]): Map[(String, String), Double] = {
    if (elements.isEmpty) {
      return Map.empty
    }
    else {
      val fec = firstElementCorrelation(elements).map(e => ((elements.head._1, e._1), e._2))
      val recCall = correlation(elements.tail)
      return (fec ++ recCall).toMap
    }
  }

  def distances(point: (Double, Double), points: Vector[(Double, Double)]): Array[Double] = {
    points.filter(p => p != point).map(p => squareDistance(p, point)).toArray
  }

  def maxDistance(xs: Vector[Double], ys: Vector[Double]): Double = {
    val points = xs zip ys
    points.map(p => distances(p, points).sum).max
  }

  def squareDistance(p1: (Double, Double), p2: (Double, Double)): Double = math.pow(p1._1 - p2._1, 2) + math.pow(p1._2 - p2._2, 2)

  def linearFit(xs: Array[Double], ys: Array[Double]): Double => Double = {
    val slope = Util.cov(xs, ys) / Util.variance(xs)
    val constantTerm = Util.avarage(ys) - slope * Util.avarage(xs)
    x => slope * x + constantTerm
  }

  def serializeLinearFunction(func: Double => Double): String = {
    val constantTerm = func(0)
    val slop = func(1) - constantTerm
    return slop + "w" + constantTerm
  }

  def deserializeLinearFunction(strFunc: String): Double => Double = {
    val split = strFunc.split("w")
    return x => split(0).toDouble * x + split(1).toDouble
  }

  def distanceFromLinearFunction(point: (Double, Double), f: Double => Double): Double = math.abs(f(point._1) - point._2)

  /**
   * Finds the biggest entropy in array when we remove one element
   *
   * @return a tuple where the first index is the max entropy and the second
   *         index is the index of the removed element.
   */
  def maxEntropy(arr: Array[Double]): (Double, Int) = {
    arr.indices
      .map(i => (removeElement(arr, i), i))
      .map(e => (entropy(e._1), e._2))
      .maxBy(_._1)
  }

  def removeElement(arr: Array[Double], idx: Int): Array[Double] = {
    arr.slice(0, idx) ++ arr.slice(idx + 1, arr.length)
  }

}
