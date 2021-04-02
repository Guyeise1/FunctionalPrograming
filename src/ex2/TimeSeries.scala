package ex2

import scala.io.Source

class TimeSeries(csvFileName:String) {

  def init(): Map[String, Vector[Double]] = {
    val file = Source.fromFile(csvFileName)
    try {
      val lineItr = file.getLines()
      val titles = lineItr.next().split(',')
      var ret = titles.map(t => (t, Vector[Double]())).toMap
      val splitLines = lineItr.map(e => e.split(','))
      splitLines.foreach( l => {
        titles.zip(l).foreach( z => {
          val v = ret(z._1) :+ z._2.toDouble
          ret = ret + (z._1 -> v)
        })
      })

      ret
    } finally {
      file.close()
    }
  }

  val features: Map[String, Vector[Double]] = init()

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


}
