object Lab2 extends App:

  // Task 1, svolto da solo

  val hello: String = "Hello, Scala"
  println(hello)

  def formattedTestString[A](expected: A, result: A): String = s"Expected: ${expected}, result: ${result}"

  def mult(x: Double, y: Double): Double = x * y

  def curriedMult(x: Double)(y: Double): Double = x * y

  val triple = curriedMult(3)
  println(formattedTestString(mult(3, 5), triple(5)))

  def divide(x: Double, y: Double): Double = x / y

  def curriedDiv(x: Double)(y: Double): Double = x / y

  val half: Double => Double = curriedDiv(_)(2)
  println(formattedTestString(divide(14, 2), half(14)))

  def newLineWithIndent(s1: String, s2: String): String = s1 + "\n\t" + s2

  def curriedNewLineWithIndent(s1: String)(s2: String): String = s1 + "\n\t" + s2

  val curriedNewLineWithIndentAsFunction: String => String => String = s1 => s2 => s1 + "\n\t" + s2
  println(formattedTestString(newLineWithIndent("match", "case"), curriedNewLineWithIndent("match")("case")))
  println(formattedTestString(newLineWithIndent("match", "case"), curriedNewLineWithIndentAsFunction("match")("case")))

  // Task 2, svolto da solo

  val positive: Int => String = _ match
    case x if x >= 0 => "positive"
    case x if x < 0 => "negative"

  def positive2(x: Int): String = x match
    case x if x >= 0 => "positive"
    case _ => "negative"

  println("\n- Positive match cases -")
  println(formattedTestString("positive", positive(12)))
  println(formattedTestString("negative", positive2(-3)))

  val empty: String => Boolean = _ == ""
  val neg: (String => Boolean) => (String => Boolean) = p => s => !p(s)

  def neg2(p: String => Boolean): String => Boolean = s => !p(s)

  val notEmpty = neg(empty)
  val notEmpty2 = neg2(empty)
  println("\n- Neg function -")
  println(formattedTestString("true", notEmpty("foo") && !notEmpty("")))
  println(formattedTestString("true", notEmpty2("foo") && !notEmpty2("")))

  def negGeneric[X](p: X => Boolean): X => Boolean = x => !p(x)

  val isZero: Int => Boolean = _ == 0
  println("\n- Neg generic function -")
  println(formattedTestString(!isZero(0), negGeneric(isZero)(0)))

  val p1: (Int, Int) => Boolean => Boolean = (x, y) => z => x <= y == z

  val p2: (Int, Int, Boolean) => Boolean = (x, y, z) => x <= y == z

  def p3(x: Int, y: Int)(z: Boolean): Boolean = x <= y == z

  def p4(x: Int, y: Int, z: Boolean): Boolean = x <= y == z

  println("\n- Currying -")
  println(formattedTestString("false", p1(2, 2)(false)))
  println(formattedTestString("true", p2(2, 2, true)))
  println(formattedTestString("true", p3(2, 2)(true)))
  println(formattedTestString("false", p4(2, 2, false)))

  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

  def composeGeneric[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))

  println("\n- Functional composition -")
  println(formattedTestString("9", compose(_ - 1, _ * 2)(5)))
  println(formattedTestString("10!", composeGeneric((x: Int) => s"${x}!", (y: Int) => y * 2)(5)))

  def composeThree[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D = x => f(composeGeneric(g, h)(x))

  println("\n- Functional composition with three functions -")
  println(formattedTestString("6!", composeThree[Int, Int, String, String](_ + "!", _.toString, _ * 2)(3)))

  // Task 3, svolto da solo

  def power(base: Double, exponent: Int): Double = exponent match
    case 0 => 1.0
    case _ => base * power(base, exponent - 1)

  def powerTail(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def _power(base: Double, exponent: Int, acc: Double): Double = exponent match
      case 0 => acc
      case _ => _power(base, exponent - 1, base * acc)

    _power(base, exponent, 1.0)

  println("\n- Recursive power function -")
  println(formattedTestString("(8.0,25.0)", (power(2, 3), power(5, 2))))
  println(formattedTestString("(9.0,64.0)", (powerTail(3, 2), power(4, 3))))

  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def _reverse(n: Int, reversed: Int): Int = n match
      case 0 => reversed
      case _ => _reverse(n / 10, reversed * 10 + (n % 10))

    _reverse(n, 0)

  println("\n- Recursive function to reverse a number -")
  println(formattedTestString("54321", reverseNumber(12345)))

  // Task 4, svolto da solo

  /**
   * Expr is a type that represents arithmetic expressions.
   * Therefore, an Expr is a sum type with three cases: Literal, Add and Multiply.
   * Literal represents a numeric constant, Add represents the addition of two sub-expressions and
   * Multiply represents the multiplication of two sub-expressions.
   *
   */
  enum Expr:
    case Literal(value: Int)
    case Add(expr1: Expr, expr2: Expr)
    case Multiply(expr1: Expr, expr2: Expr)

  object Expr:
    /**
     * evaluate returns the numerical result of the expression passed, if the expression is:
     * - Literal it returns its value.
     * - Add it returns the addition of the evaluation of the two sub-expressions.
     * - Multiply it returns the multiplication of the evaluation of the two sub-expressions.
     *
     * @param expr the expression to evaluate.
     * @return the result of the evaluation of the expression passed.
     */
    def evaluate(expr: Expr): Int = expr match
      case Literal(value) => value
      case Add(expr1, expr2) => evaluate(expr1) + evaluate(expr2)
      case Multiply(expr1, expr2) => evaluate(expr1) * evaluate(expr2)

    /**
     * show returns the String that represents the expression passed, if the expression is:
     * - Literal the string corresponds to its value.
     * - Add the string is like (expression1 + expression2).
     * - Multiply the string is like (expression1 * expression2).
     *
     * @param expr the expression to represents.
     * @return the string representation of the expression passed.
     */
    def show(expr: Expr): String = expr match
      case Literal(value) => value.toString
      case Add(expr1, expr2) => s"(${show(expr1)} + ${show(expr2)})"
      case Multiply(expr1, expr2) => s"(${show(expr1)} * ${show(expr2)})"