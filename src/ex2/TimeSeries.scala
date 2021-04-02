

class TimeSeries(csvFileName:String) {

  val features=???


  // given name of a feature return in O(1) its value series
  def getValues(feature:String):Option[Vector[Double]]=???

  // given name of a feature return in O(1) its value at the given time step
  def getValue(feature:String,timeStep:Int):Option[Double]=???

  // given name of a feature return its value series in the range of indices
  def getValues(feature:String,r:Range):Option[Vector[Double]]=???


}
