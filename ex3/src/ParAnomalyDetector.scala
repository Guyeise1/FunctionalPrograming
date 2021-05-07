import java.util.concurrent.ExecutorService
import scala.collection.mutable.ListBuffer


case class Report(feature: String, var timeStep: Int, anomalyScore: Double)


trait ParAnomalyDetector {
  type Reports = ListBuffer[Report]
  def map(ts: TimeSeries): Reports
  def reduce(r1: Reports, r2: Reports): Reports

  // implement
  def detect(ts: TimeSeries, es: ExecutorService, chunks: Int): Vector[Report] = {
    val tss = ts.split(chunks)
    val chunkSize = tss.head.length
    tss.map(t => es.submit(() => this.map(t)))
      .map(f => f.get())
      .zipWithIndex
      .map(e => {
        val reports = e._1
        val i = e._2
        reports.toList.foreach(r => r.timeStep = r.timeStep + (i * chunkSize))
        reports
      })
      .reduce(this.reduce).toList.toVector
  }
}

