import scala.math.{abs, sqrt}

class Complex(val re : Double = 0, val im : Double = 0){

  val modulus = sqrt(re * re + im * im)

  override def toString: String =
    if(im == 0){
      if(re == 0) "0" else s"$re"
    }
    else {
      if(re == 0) s"$im.i" else
        if(im < 0) re + " - " + abs(im) + ".i" else s"$re + $im.i"
    }

  def +(other : Complex) =
    new Complex(re + other.re, im + other.im)

  def unary_-() =
    new Complex(-re, -im)

  def -(other : Complex) =
    this + -other

  def *(other : Complex) =
    new Complex(re * other.re - im * other.im, re * other.im + im * other.re)

  def /(other : Complex) = {
    if(other.modulus != 0){
      new Complex((re * other.re + im * other.im) / (other.modulus * other.modulus), (other.re * im - other.im * re) / other.modulus * other.modulus)
    }
    else{
      "Division by zero"
    }
  }

  def <(other : Complex) =
    if(modulus < other.modulus) true else false

  def ==(other : Complex) =
    if(re == other.re && im == other.im) true else false

  def maxModulus(other : Complex) =
    if(modulus >= other.modulus) this else other
}

object Complex {
  def apply(re : Double, im : Double) = new Complex(re, im)
  implicit def double2Complex(x : Double) : Complex= new Complex(x)
}

