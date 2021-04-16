import scala.collection.{immutable, mutable}

object HybridAnomalyDetector extends  AnomalyDetector {



  override def learn(normal: TimeSeries): Map[String, String] = {
    def serialize(modelHighCorrelation: Array[((String, String),Double=>Double, Double)],
                  modelLowCorrelation: Array[((String, String), Double)],
                  modelNoCorrelation: Array[(String, Double)]): Map[String, String] = {
      val sHigh = modelHighCorrelation.map(e => List(e._1._1, e._1._2,Util.serializeLinearFunction(e._2), e._3).mkString(",")).mkString(" ")
      val sLow = modelLowCorrelation.map( e => List(e._1._1, e._1._2, e._2).mkString(",")).mkString(" ")
      val sNo = modelNoCorrelation.map( e => List(e._1, e._2).mkString(",")).mkString(" ")

      Map("high" -> sHigh, "low" -> sLow, "no" -> sNo)
    }

    val correlation = Util.correlation(normal.features).map(e => (e._1, math.abs(e._2)))
    val highCorrelation = correlation.filter(e => e._2 >= 0.9)
    val lowCorrelation = correlation.filter(e => e._2 < 0.9 && e._2 > 0.5)
    val noCorrelation = correlation.filter( e => e._2 <= 0.5).keySet.flatMap( x => List(x._1, x._2)).toArray.distinct

    val modelHighCorrelation = highCorrelation.map(e => {
      val x = e._1._1
      val y = e._1._2
      val xs = normal.features(x).toArray
      val ys = normal.features(y).toArray
      val points = xs zip ys
      val f = Util.linearFit(xs, ys)
      val limit = points.map(p => Util.distanceFromLinearFunction(p, f)).max
      ((x,y),f, limit)
    }).toArray

    val modelLowCorrelation = lowCorrelation.map( e => {
      val x = e._1._1
      val y = e._1._2
      val xs = normal.features(x)
      val ys = normal.features(y)
      val points = xs zip ys

      val radius = points.map(p => {
         points.map(o => Util.squareDistance(p, o)).max
      }).min

      ((x,y), radius)
    }).toArray

    val modelNoCorrelation = noCorrelation.map( x => {
      val xs = normal.features(x)
      (x, xs.map( e => Util.zscore(xs.toArray, e)).max)
    })


    serialize(modelHighCorrelation, modelLowCorrelation, modelNoCorrelation)
  }


  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    def deserialize(model: Map[String, String]): (
      Array[((String, String),Double=>Double, Double)],
      Array[((String, String), Double)],
      Array[(String, Double)]) = {

      val lowModel = model("low").split(" ").map( e => e.split(",")).filter(x => x(0) != "").map(e => ((e(0), e(1)), e(2).toDouble))
      val noModel = model("no").split(" ").map(e => e.split(",")).filter(x => x(0) != "").map(e => (e(0), e(1).toDouble))
      val highModel = model("high").split(" ").map(e=> e.split(",")).filter(x => x(0) != "").map( e => ((e(0), e(1)), Util.deserializeLinearFunction(e(2)), e(3).toDouble))
      (highModel, lowModel, noModel)

    }
    val (highCorrelation, lowCorrelation, noCorrelation) = deserialize(model)

    val highCorrelationResult = LinearRegAnomalyDetector.detect(highCorrelation, test)
    val lowCorrelationResult = lowCorrelation.flatMap( e => {
      val (x,y) = e._1
      val (xs,ys) = (test.features(x).toArray, test.features(y).toArray)
      val points = xs zip ys
      val limit = e._2
      val center = points.minBy( p => points.map( o => Util.squareDistance(p,o)).max)
      val indexedPoints = points zip points.indices
      indexedPoints.filter( p => Util.squareDistance(p._1, center) > limit).map(t => (x + "," + y, t._2))
    }).toVector
    val noCorrelationResult = noCorrelation.flatMap( e => {
      val xs = test.features(e._1).toArray
      val limit = e._2
      val indexedXS = xs zip xs.toList.indices
      val badIndexedXS = indexedXS.filter( x => Util.zscore(xs, x._1) > limit)
      badIndexedXS.map(t => (e._1,t._2))
    }).toVector

    highCorrelationResult concat lowCorrelationResult concat noCorrelationResult
  }

}
