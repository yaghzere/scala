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

c1
c2
c3
c4
c5
c6
c1.modulus
c3.modulus
c7
c8
c9
c10
c11
c12
c13
comparison1
comparison2
equality1
equality2
higherModulus