

object SumSqrAnomalyDetector extends  AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    def serialize(model: Array[((String,String), Double)]): Map[String, String] = {
      val keys = model.map(e => e._1).map(e => e._1 + "," + e._2).mkString(" ")
      val limits = model.map(e => e._2.toString).mkString(" ")
      return Map("keys" -> keys, "limits" -> limits)
    }

    val correlative = Util.correlation(normal.features).filter(e => e._2 > 0.9).keySet.toArray
    val distances = correlative.map( e => {
      val a = normal.features(e._1)
      val b = normal.features(e._2)
      Util.maxDistance(a,b)
    })
    serialize(correlative zip distances)
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    def deserialize(model: Map[String, String]): Array[((String,String), Double)] = {
      val keys = model("keys").split(" ").map(x => x.split(",")).map(c => (c(0), c(1)))
      val limits = model("limits").split(" ").map(l => l.toDouble)
      keys zip limits
    }

    val myModel = deserialize(model)
    myModel.flatMap( e => {
      val x = e._1._1
      val y = e._1._2
      val limit = e._2
      val xs = test.features(x)
      val ys = test.features(y)
      val points = xs zip ys
      val indexedPoints = points.indices zip points
      val indexedBadPoints = indexedPoints.filter(ip => Util.distances(ip._2, points).sum > limit)
      indexedBadPoints.map(ibp => (x+","+y, ibp._1))
    }).toVector
  }
}
