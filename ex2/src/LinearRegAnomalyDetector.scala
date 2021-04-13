import scala.annotation.tailrec
import scala.collection.mutable

object LinearRegAnomalyDetector extends  AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    def firstElementCorrelation(elements: Map[String, Vector[Double]]): Array[(String, Double)] = {
        elements.tail.map(e => (e._1,
          Util.cov(elements.head._2.toArray, e._2.toArray)
            / Util.standardDeviation(e._2.toArray)
            / Util.standardDeviation(elements.head._2.toArray)
        )).toArray
      }

    def totalCorrelation(elements: Map[String, Vector[Double]]): Map[(String, String), Double] = {
      if(elements.isEmpty) {
        return Map.empty
      }
      else {
        val fec = firstElementCorrelation(elements).map(e => ((elements.head._1, e._1), e._2)).toMap
        val recCall = totalCorrelation(elements.tail)
        return fec.concat(recCall)
      }
    }

    def linearFit(a: Array[Double], b: Array[Double]) : Double => Double = {
      val slope = Util.cov(a, b) / Util.variance(a)
      val constantTerm = Util.avarage(b) - slope * Util.avarage(a)
      return (x => slope * x + constantTerm)
    }

    def maxDist(linearFunction: Double => Double, xs: Array[Double], ys: Array[Double]): Double = {
      val points = xs zip ys
      points.map(p => math.pow(linearFunction(p._1) - p._2, 2)).max
    }

    val correlativeFeatures = totalCorrelation(normal.features).filter(e => math.abs(e._2) >= 0.9).keySet

    val linearFunctions = correlativeFeatures.map(k => (k, linearFit(normal.features(k._1).toArray, normal.features(k._2).toArray)))

    linearFunctions.map(e => (e._1,e._2, maxDist(e._2, normal.features(e._1._1).toArray, normal.features(e._1._2).toArray)))

  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
  }
}
