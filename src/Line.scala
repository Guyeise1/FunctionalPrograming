import Util.{cov, mu, variance}

class Line(ps: Array[Point]) {
  private val x = ps.map(p => p.x)
  private val y = ps.map(p => p.y)
  // read only values a and b
  val a: Double = cov(x, y) / variance(x)
  val b: Double = mu(y) - a * mu(x)

  // f
  def f(x: Double): Double = a * x + b

  // dist
  def dist(p: Point): Double = math abs (f(p.x) - p.y)
}
