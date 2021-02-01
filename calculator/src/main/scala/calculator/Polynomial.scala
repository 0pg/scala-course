package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val deltaSqrt = Math.sqrt(delta())
      if (delta() < 0) Set.empty
      else if (delta() == 0) Set(-b() / 2 * a())
      else Set((-b() + deltaSqrt) / (2 * a()), (-b() - deltaSqrt) / (2 * a()))
    }
  }
}
