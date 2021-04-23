

object LinearRegAnomalyDetector extends  AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    def maxDist(linearFunction: Double => Double, xs: Array[Double], ys: Array[Double]): Double = {
      val points = xs zip ys
      points.map(p => math.abs(linearFunction(p._1) - p._2)).max
    }

    def serialize(model: Array[((String,String), Double=>Double, Double)]): Map[String, String] = {
      val keys = model.map( e => e._1).map( e => e._1 + "," + e._2).mkString(" ")
      val limits = model.map(e => e._3.toString).mkString(" ")
      val functions = model.map(e => Util.serializeLinearFunction(e._2)).mkString(" ")
      return Map("keys" -> keys, "limits" -> limits, "functions" -> functions)
    }

    val correlativeFeatures = Util.correlation(normal.features).filter(e => math.abs(e._2) >= 0.9).groupBy(e => e._1).values.map(l => l.maxBy(t => t._2)).map(e => e._1)

    val linearFunctions = correlativeFeatures.map(k => (k, Util.linearFit(normal.features(k._1).toArray, normal.features(k._2).toArray)))

    val ret = linearFunctions.map(e => (e._1,e._2, maxDist(e._2, normal.features(e._1._1).toArray, normal.features(e._1._2).toArray))).toArray

    return serialize(ret)
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    def deserialize(model: Map[String, String]): Array[((String,String), Double=>Double, Double)] = {
      val keys = model("keys").split(" ").map(p => p.split(",")).filter(x => x(0)!= "").map(p => (p(0), p(1))): Array[(String, String)]
      val limits = model("limits").split(" ").filter(x => x!= "").map(l => l.toDouble): Array[Double]
      val functions = model("functions").split(" ").filter(x => x!= "").map(f => Util.deserializeLinearFunction(f))

      (keys zip functions zip limits).map(e => (e._1._1, e._1._2, e._2))

    }
    val s_model = deserialize(model)
    detect(s_model, test)
  }

  def detect(model: Array[((String,String), Double=>Double, Double)], test: TimeSeries): Vector[(String, Int)] = {
    model.flatMap(e => {
      val a = e._1._1
      val b = e._1._2
      val func = e._2
      val limit = e._3
      val points = test.features(a).toList.indices zip (test.features(a) zip test.features(b))
      val bad_points = points.filter(p => math.abs(func(p._2._1) - p._2._2) > limit)
      bad_points.map(p => (a + "," + b, p._1)).toVector
    }).toVector
  }
}
