import scala.collection.mutable.ListBuffer

object EntropyAnomalyDetector extends ParAnomalyDetector{


  override def map(ts: TimeSeries):Reports= {
    ListBuffer.empty ++ ts.features
      .map(f => {
        val key = f._1
        val data = f._2
        val result = Util.maxEntropy(data.toArray)
        Report(key, result._2, result._1)
      })
  }

  override def reduce(r1:Reports,r2:Reports):Reports = {
    ListBuffer.empty ++ (r1 ++ r2).groupBy(r => r.feature).map(e => e._2.maxBy(r => r.anomalyScore))
  }
}