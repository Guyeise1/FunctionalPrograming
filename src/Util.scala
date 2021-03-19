object Util {


  // max
  def max[A](lst: List[A], comparator: (A, A) => Int): A = {
    if(lst.length == 1) {
      lst.head
    } else {
      val (l1, l2) = lst.splitAt(lst.length/2)
      val (r1,r2) = (max(l1, comparator), max(l2,comparator))
      if(comparator(r1,r2) > 0) {
        r1
      } else {
        r2
      }
    }
  }
	// map
  def map[A,B,C](lst: List[A], f: A=>B, g: B=>C) :List[C] = {
    lst.map(a => g(f(a)))
  }
	// isSorted
  def isSorted[A](lst: List[A], comparator: (A,A) => Boolean): Boolean = {
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
    probs(arr).map(p => -p * Math.log10(p) / Math.log10(2)).sum
  }

	// mu
  def mu(arr: Array[Double]): Double = {
    arr.zip(probs(arr)).map(x => x._1 * x._2).sum
  }

  // variance
  def variance(arr: Array[Double]): Double = {
    val m = mu(arr)
    (arr zip probs(arr)).map(x => x._2 * (x._1 - m) * (x._1 - m)).sum
  }

	// zscore
  def zscore(arr: Array[Double], x: Double) : Double = {
    (x - mu(arr)) / math.sqrt(variance(arr))
  }
	// cov

	// pearson
}
