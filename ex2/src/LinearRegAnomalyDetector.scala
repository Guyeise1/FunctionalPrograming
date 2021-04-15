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
      x => slope * x + constantTerm
    }

    def maxDist(linearFunction: Double => Double, xs: Array[Double], ys: Array[Double]): Double = {
      val points = xs zip ys
      points.map(p => math.abs(linearFunction(p._1) - p._2)).max
    }

    def serialize(model: Array[((String,String), Double=>Double, Double)]): Map[String, String] = {
      def serializeLinearFunction(func: Double=> Double): String = {
        val constantTerm = func(0)
        val slop = func(1) - constantTerm
        return slop + "," + constantTerm
      }
      val keys = model.map( e => e._1).map( e => e._1 + "," + e._2).reduce((x1,x2) => x1 + " " + x2)
      val limits = model.map(e => e._3.toString).reduce((x1,x2) => x1 + " " + x2)
      val functions = model.map(e => serializeLinearFunction(e._2)).reduce((f1,f2) => f1 + " " + f2)
      return Map("keys" -> keys, "limits" -> limits, "functions" -> functions)
    }

    val correlativeFeatures = totalCorrelation(normal.features).filter(e => math.abs(e._2) >= 0.9).keySet

    val linearFunctions = correlativeFeatures.map(k => (k, linearFit(normal.features(k._1).toArray, normal.features(k._2).toArray)))

    val ret = linearFunctions.map(e => (e._1,e._2, maxDist(e._2, normal.features(e._1._1).toArray, normal.features(e._1._2).toArray))).toArray

    return serialize(ret)
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    def deserialize(model: Map[String, String]): Array[((String,String), Double=>Double, Double)] = {
      def deserializeLinearFunction(strFunc: String): Double => Double = {
        val split = strFunc.split(",")
        return x=> split(0).toDouble * x + split(1).toDouble
      }
      val keys = model("keys").split(" ").map(p => p.split(",")).map(p => (p(0), p(1))): Array[(String, String)]
      val limits = model("limits").split(" ").map(l => l.toDouble): Array[Double]
      val functions = model("functions").split(" ").map(f => deserializeLinearFunction(f)): Array[Double => Double]

      (keys zip functions zip limits).map(e => (e._1._1, e._1._2, e._2))

    }
    val s_model = deserialize(model)
    s_model.map(e => {
      val a = e._1._1
      val b = e._1._2
      val func = e._2
      val limit = e._3
      val points = test.features(a).toList.indices zip (test.features(a) zip test.features(b))
      val bad_points = points.filter( p => math.abs(func(p._2._1) - p._2._2) > limit)
      bad_points.map( p => (a +"," +b, p._1)).toVector
    }).reduce( (x1,x2) => x1 concat x2)



  }
}
