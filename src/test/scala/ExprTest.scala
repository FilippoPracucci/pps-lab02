import Lab2.Expr
import Lab2.Expr.{Add, Literal, Multiply, show}
import org.junit.*
import org.junit.Assert.*

class ExprTest:
  val expr1 = Expr.Literal(10)
  val expr2 = Expr.Literal(5)

  @Test def testEvaluateLiteral(): Unit =
    val expectedResult = 10
    assertEquals(expectedResult, Expr.evaluate(expr1))

  @Test def testEvaluateAddLiteral(): Unit =
    val expectedResult = 15
    assertEquals(expectedResult, Expr.evaluate(Expr.Add(expr1, expr2)))

  @Test def testEvaluateMultiplyLiteral(): Unit =
    val expectedResult = 50
    assertEquals(expectedResult, Expr.evaluate(Expr.Multiply(expr1, expr2)))

  @Test def testShowLiteralExpression(): Unit =
    val expectedString = "10"
    assertEquals(expectedString, Expr.show(expr1))

  @Test def testShowAddExpression(): Unit =
    val expectedString = "(10 + 5)"
    assertEquals(expectedString, Expr.show(Expr.Add(expr1, expr2)))

  @Test def testShowMultiplyExpression(): Unit =
    val expectedString = "(10 * 5)"
    assertEquals(expectedString, Expr.show(Expr.Multiply(expr1, expr2)))