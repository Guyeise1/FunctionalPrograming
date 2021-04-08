import scala.collection.mutable

object ZAnomalyDetector extends AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    normal.features.map( e =>
      (e._1,e._2.map(x => Math.abs(Util.zscore(e._2.toArray, x))).max.toString))
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    val zScores = test.features.map(e => (e._1, e._2.map( x => Math.abs(Util.zscore(e._2.toArray, x)))))
    val errors = zScores.map(e => (e._1, e._2.map(x => if (x > model(e._1).toDouble) 1 else 0)))
    val errorIndexes = errors.map( e => (e._1, e._2.zip(e._2.indices).filter(x => x._1 == 1).map(x=> x._2)))
    errorIndexes.map( e => (e._1, e._2.map( x => (e._1, x)))).values.reduce( (x,y) => x ++ y)
  }
}
