import Lab2.Expr
import Lab2.Expr.{Add, Multiply}
import org.junit.*
import org.junit.Assert.*

class ExprTest:
  val expr1 = Expr.Literal(10)
  val expr2 = Expr.Literal(5)

  @Test def evaluateLiteral(): Unit =
    assertEquals(10, Expr.evaluate(expr1))

  @Test def evaluateAddLiteral(): Unit =
    assertEquals(15, Expr.evaluate(Add(expr1, expr2)))

  @Test def evaluateMultiplyLiteral(): Unit =
    assertEquals(50, Expr.evaluate(Multiply(expr1, expr2)))