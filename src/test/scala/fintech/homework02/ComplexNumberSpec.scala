package fintech.homework02
import org.scalatest.{FlatSpec, Matchers}

class ComplexNumberSpec extends FlatSpec with Matchers {
  val eps = 1e-5

  "Sum of complex numbers" should "be equal to (a+bi)+(c+di)=(a+c)+(b+d)" in {
    val a = new ComplexNumber(1, 2)
    val b = new ComplexNumber(3, 4)
    val sumDif = (a + b) + new ComplexNumber(-4, -6)
    math.abs(sumDif.real) should be <= eps
    math.abs(sumDif.image) should be <= eps
  }

  "Sum of complex numbers" should "be commutative" in {
    val a = new ComplexNumber(1, 2)
    val b = new ComplexNumber(3, 4)
    val negA = new ComplexNumber(-1, -2)
    val negB = new ComplexNumber(-3, -4)
    val sumDif = (a + b) + (negB + negA)
    math.abs(sumDif.real) should be <= eps
    math.abs(sumDif.image) should be <= eps
  }

  "Sum of complex numbers" should "be associative" in {
    val a = new ComplexNumber(1, 2)
    val b = new ComplexNumber(3, 4)
    val c = new ComplexNumber(5, 6)
    val negA = new ComplexNumber(-1, -2)
    val negB = new ComplexNumber(-3, -4)
    val negC = new ComplexNumber(-5, -6)
    val sumDif = ((a + b) + c) + (negA + (negB + negC))
    math.abs(sumDif.real) should be <= eps
    math.abs(sumDif.image) should be <= eps
  }

  "Sum of opposite complex numbers" should "be equal to zero" in {
    val a = new ComplexNumber(1, 2)
    val b = new ComplexNumber(-1, -2)
    val sum = a + b
    math.abs(sum.real) should be <= eps
    math.abs(sum.image) should be <= eps
  }

  "Product of complex numbers" should "be equal to (a+bi)*(c+di)=(ac-bd)+(bc+ad)i" in {
    val a = new ComplexNumber(1, 2)
    val b = new ComplexNumber(3, 4)
    val negA = new ComplexNumber(-1, -2)
    val productDif = a * b + negA * b
    math.abs(productDif.real) should be <= eps
    math.abs(productDif.image) should be <= eps
  }

  "Product of complex numbers" should "be commutative" in {
    val a = new ComplexNumber(1, 2)
    val b = new ComplexNumber(3, 4)
    val negA = new ComplexNumber(-1, -2)
    val productDif = a * b + b * negA
    math.abs(productDif.real) should be <= eps
    math.abs(productDif.image) should be <= eps
  }

  "Product of complex numbers" should "be associative" in {
    val a = new ComplexNumber(1, 2)
    val b = new ComplexNumber(3, 4)
    val c = new ComplexNumber(5, 6)
    val negA = new ComplexNumber(-1, -2)
    val productDif = (a * b) * c + negA * (b * c)
    math.abs(productDif.real) should be <= eps
    math.abs(productDif.image) should be <= eps
  }

  "Product of complex number and zero" should "be equal to zero" in {
    val a = new ComplexNumber(1, 2)
    val zero = new ComplexNumber(0, 0)
    val product = a * zero
    math.abs(product.real) should be (0)
    math.abs(product.image) should be (0)
  }

  "Product of complex number and one" should "be equal to this complex number" in {
    val a = new ComplexNumber(1, 2)
    val one = new ComplexNumber(1, 0)
    val product = a * one
    math.abs(product.real) should be (a.real)
      math.abs(product.image) should be (a.image)
  }

  "Product of complex numbers" should "be distributive" in {
    val a = new ComplexNumber(1, 2)
    val b = new ComplexNumber(3, 4)
    val c = new ComplexNumber(5, 6)
    val negA = new ComplexNumber(-1, -2)
    val productDif = a * (b + c) + negA * b + negA * c
    math.abs(productDif.real) should be <= eps
    math.abs(productDif.image) should be <= eps
  }

  "Complex number in power" should "be equal to (cos(phi)+i*sin(phi))^n=r^n(cos(n*phi)+i*sin(n*phi))" in {
    val a = new ComplexNumber(1, 2)
    val powerDif = a ~ 2 + new ComplexNumber(3, -4)
    math.abs(powerDif.real) should be <= eps
    math.abs(powerDif.image) should be <= eps
  }

  "Complex number in negative power" should "be equal to (cos(phi)+i*sin(phi))^n=r^n(cos(n*phi)+i*sin(n*phi))" in {
    val a = new ComplexNumber(1, 2)
    val powerDif = a ~ -2 + new ComplexNumber(0.12, 0.16)
    math.abs(powerDif.real) should be <= eps
    math.abs(powerDif.image) should be <= eps
  }

  "Equivalent complex numbers" should "have equivalent real and imaginary parts" in {
    val a = new ComplexNumber(-1, 2.3)
    val b = new ComplexNumber(-1, 2.3)
    a == b should be (true)
  }

  "Equivalent complex numbers" should "have equivalent hash codes" in {
    val a = new ComplexNumber(-1, 2.3)
    val b = new ComplexNumber(-1, 2.3)
    a.hashCode == b.hashCode should be (true)
  }

  "Nonequivalent complex numbers" should "have different real and imaginary parts" in {
    val a = new ComplexNumber(-1, 2.3)
    val b = new ComplexNumber(1, 2.3)
    a == b should be (false)
  }

  "String representation of a complex number" should "be equal to \"a + bi\"" in {
    val a = new ComplexNumber(1, 2)
    a.toString == "1.0 + 2.0i" should be (true)
  }

  "String representation of a negative complex number" should "be equal to \"a - bi\"" in {
    val a = new ComplexNumber(1, -2)
    a.toString == "1.0 - 2.0i" should be (true)
  }

  "String representation of a complex number without real part" should "be equal to \"bi\"" in {
    val a = new ComplexNumber(0, -2)
    a.toString == "-2.0i" should be (true)
  }

  "String representation of a complex number without imaginary part" should "be equal to \"a\"" in {
    val a = new ComplexNumber(1, 0)
    a.toString == "1.0" should be (true)
  }
}
