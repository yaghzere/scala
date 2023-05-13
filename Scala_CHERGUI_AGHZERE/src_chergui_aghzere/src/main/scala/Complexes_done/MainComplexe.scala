class MainComplexe {

  def main() = {
    val c1 = new Complex()
    val c2 = new Complex(2)
    val c3 = new Complex(3, 4)
    val c4 = new Complex(0, 5)
    val c5 = new Complex(-1, -2)
    val c6 = Complex(3, 4)
    val c7 = c6 + c5
    val c8 = -c5
    val c9 = c3 - c4
    val c10 = c5 * c6
    val c11 = c5 / c6
    val c12 = c5 / c1
    val x = 2
    val c13 = x * c6
    val comparison1 = c3 < c5
    val comparison2 = c5 < c3
    val equality1 = c3 == c5
    val equality2 = c3 == c6
    val higherModulus = c3.maxModulus(c5)

    // Displayed values respect their declaration order

    c1.toString()
    c2.toString()
    c3.toString()
    c4.toString()
    c5.toString()
    c6.toString()
    println(c1.modulus)
    println(c3.modulus)
    c7.toString()
    c8.toString()
    c9.toString()
    c10.toString()
    c11.toString()
    c12.toString()
    c13.toString()
    println(comparison1)
    println(comparison2)
    println(equality1)
    println(equality2)
    higherModulus.toString()
  }

}

