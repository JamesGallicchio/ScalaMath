object ScalaMath {

  sealed trait Expression {
    def applyInner(f: Expression => Expression): Expression
  }


  case class Add(exp1: Expression, exp2: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Add(f(exp1), f(exp2))
    override def toString: String = "(" + exp1 + ")+(" + exp2 + ")"
  }

  case class Negate(exp: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Negate(f(exp))
    override def toString: String = "-(" + exp + ")"
  }

  case class Multiply(exp1: Expression, exp2: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Multiply(f(exp1), f(exp2))
    override def toString: String = "(" + exp1 + ")*(" + exp2 + ")"
  }

  case class Reciprocate(exp: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Reciprocate(f(exp))
    override def toString: String = "1/(" + exp + ")"
  }

  case class Power(base: Expression, power: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Power(f(base), f(power))
    override def toString: String = "(" + base + ")^(" + power + ")"
  }

  case class Log(base: Expression, exp: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Log(f(base), f(exp))
    override def toString: String = "log_" + base + "(" + exp + ")"
  }


  case class Sum(control: Variable, start: Expression, end: Expression, func: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Sum(control, f(start), f(end), f(func))
    override def toString: String = "sum(" +
                                    control + "=" + start + " -> " + end + ", " +
                                    func + ")"
  }


  case class Product(control: Variable, start: Expression, end: Expression, func: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Product(control, f(start), f(end), f(func))
    override def toString: String = "product(" +
                                    control + "=" + start + " -> " + end + ", " +
                                    func + ")"
  }

  case class Factorial(exp: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Factorial(f(exp))
    override def toString: String = "(" + exp + ")!"
  }


  case class Sine(ang: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Sine(f(ang))
    override def toString: String = "sin(" + ang + ")"
  }

  case class Cosine(ang: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Cosine(f(ang))
    override def toString: String = "cos(" + ang + ")"
  }


  case class Tangent(ang: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Tangent(f(ang))
    override def toString: String = "tan(" + ang + ")"
  }


  case class Arcsine(ang: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Arcsine(f(ang))
    override def toString: String = "asin(" + ang + ")"
  }


  case class Arccosine(ang: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Arccosine(f(ang))
    override def toString: String = "acos(" + ang + ")"
  }


  case class Arctangent(ang: Expression) extends Expression {
    override def applyInner(f: (Expression) => Expression) = Arctangent(f(ang))
    override def toString: String = "atan(" + ang + ")"
  }





  case class Variable(symbol: String) extends Expression {
    override def applyInner(f: (Expression) => Expression): Variable = this
    override def toString: String = symbol
  }




  trait Number extends Expression {
    override def applyInner(f: (Expression) => Expression): Number = this

    def +(n: Number): Number
    def -(n: Number): Number
    def *(n: Number): Number
    def /(n: Number): Number
    def ^(n: Number): Number
    def >(n: Number): Boolean
    def <(n: Number): Boolean
    def >=(n: Number): Boolean
    def <=(n: Number): Boolean
  }

  final case object IMAGINARY extends Expression {
    override def applyInner(f: (Expression) => Expression) = IMAGINARY
    override def toString: String = "i"
  }




  type Reducer = PartialFunction[Expression, Expression]

  def reduce(exp: Expression): Expression = {
    def r(e: Expression): Expression = (AlgebraReducers.AddReducer
      orElse AlgebraReducers.MultReducer
      orElse AlgebraReducers.RecipReducer
      orElse PartialFunction[Expression, Expression](o => o))(e)

    var last = exp.applyInner(r)
    var current = r(last)
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