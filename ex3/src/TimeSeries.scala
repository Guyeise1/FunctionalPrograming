
import scala.collection.immutable.ListMap
import scala.io.Source
private object Initiator {
  def init(csvFileName: String): Map[String, Vector[Double]] = {
    val file = Source.fromFile(csvFileName)
    try {
      val lineItr = file.getLines()
      val titles = lineItr.next().split(',')
      var values = titles.map(t => (t, Vector[Double]())).toMap
      val splitLines = lineItr.map(e => e.split(','))
      splitLines.foreach( l => {
        titles.zip(l).foreach( z => {
          val v = values(z._1) :+ z._2.toDouble
          values = values + (z._1 -> v)
        })
      })

      var orderedValues: ListMap[String, Vector[Double]] = new ListMap
      titles.foreach( t => orderedValues = orderedValues + (t -> values(t)))
      return orderedValues
    } finally {
      file.close()
    }
  }
}

class TimeSeries(val features: Map[String, Vector[Double]]) {
  def this(csvFileName: String) {
    this(Initiator.init(csvFileName))
  }


  // given name of a feature return in O(1) its value series
  def getValues(feature:String):Option[Vector[Double]]= features.get(feature)

  // given name of a feature return in O(1) its value at the given time step
  def getValue(feature:String,timeStep:Int):Option[Double]= {
    if(features.contains(feature)) {
      if(features(feature).size > timeStep) {
        return Option apply features(feature)(timeStep)
      }
    }
    None
  }

  // given name of a feature return its value series in the range of indices
  def getValues(feature:String,r:Range):Option[Vector[Double]]= {
    if(features.contains(feature)) {
      if(features(feature).size > r.end && r.start >= 0) {
        return Option apply features(feature).slice(r.start, r.end + 1)
      }
    }
    None
  }


  def length: Int = this.features.values.iterator.next().size

  def split(n: Int): List[TimeSeries] = {
    return cut(this.length / n)

  }

  /**
   * Split the TimeSeries to group of n elements (the last one contains up to 2n elements)
   * @param n - group size
   */
  def cut(n: Int): List[TimeSeries] = {
    def cut(ts: TimeSeries, n: Int): List[TimeSeries] = {
      if(ts.length == 0) {
        return List()
      }
      if (ts.length < 2 * n) {
        return List(ts)
      }

      val head = new TimeSeries(ts.features.map(e => (e._1, e._2.slice(0, n))))
      val tail = cut(new TimeSeries(ts.features.map(e => (e._1, e._2.slice(n, ts.length)))), n)
      return head :: tail
    }

    return cut(this, n)
  }
}
