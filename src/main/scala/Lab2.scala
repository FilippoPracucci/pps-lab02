object Lab2 extends App:

  // Task 1, svolto da solo

  val hello: String = "Hello, Scala"
  println(hello)


  // Task 4, svolto da solo

  enum Expr:
    case Literal(value: Int)
    case Add(expr1: Expr, expr2: Expr)
    case Multiply(expr1: Expr, expr2: Expr)

  object Expr:
    def evaluate(expr: Expr): Int = expr match
      case Literal(value) => value
      case Add(expr1, expr2) => evaluate(expr1) + evaluate(expr2)
      case Multiply(expr1, expr2) => evaluate(expr1) * evaluate(expr2)
    def show(expr: Expr): String = expr match
      case Literal(value) => value.toString
      case Add(expr1, expr2) => "(" + show(expr1) + " + " + show(expr2) + ")"
      case Multiply(expr1, expr2) => "(" + show(expr1) + " * " + show(expr2) + ")"