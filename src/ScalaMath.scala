import Numbers.BasicInteger
import ScalaMath.Add

object ScalaMath {

  sealed trait Expression


  case class Add(exp1: Expression, exp2: Expression) extends Expression

  case class Negate(exp: Expression) extends Expression

  case class Multiply(exp1: Expression, exp2: Expression) extends Expression

  case class Reciprocate(exp: Expression) extends Expression

  case class Power(base: Expression, power: Expression) extends Expression

  case class Log(base: Expression, exp: Expression) extends Expression


  case class Sum(control: Variable, start: Expression, end: Expression, func: Expression)

  case class Product(control: Variable, start: Expression, end: Expression, func: Expression)

  case class Factorial(exp: Expression) extends Expression


  case class Sine(ang: Expression) extends Expression

  case class Cosine(ang: Expression) extends Expression

  case class Tangent(ang: Expression) extends Expression

  case class Arcsine(ang: Expression) extends Expression

  case class Arccosine(ang: Expression) extends Expression

  case class Arctangent(ang: Expression) extends Expression




  case class Variable(symbol: String) extends Expression



  trait Number extends Expression {

    def +(n: Number): Number
    def -(n: Number): Number
    def *(n: Number): Number
    def /(n: Number): Number
    def ^(n: Number): Number
  }




  type Reducer = PartialFunction[Expression, Expression]

  def Reducer[T <: Expression](reducer: T => Expression): PartialFunction[Expression, Expression] = {
    case exp: T => reducer(exp)
  }

  def reduce(exp: Expression): Expression = {
    def r(e: Expression): Expression = (AlgebraReducers.AddReducer orElse AlgebraReducers.MultReducer)(e)

    var last = exp
    var current = r(exp)
    while(current != last) {
      last = current
      current = r(current)
    }
    current
  }

  def evaluate(exp: Expression, variables: Map[Variable, Number]): Expression = {

    reduce(exp)
  }
}