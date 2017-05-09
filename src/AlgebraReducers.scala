import Numbers.BasicInteger
import ScalaMath._

object AlgebraReducers {

  val AddReducer: Reducer = {
    case Add(n1: Number, n2: Number) => n1 + n2
    case Add(BasicInteger(0), e) => e
    case Add(e, BasicInteger(0)) => e
    case Add(e1, e2) if CommonFactor.of(e1, e2) isInstanceOf[Some] => CommonFactor.of(e1, e2)
  }

  val MultReducer: Reducer = { case mult: Multiply => (reduce(mult.exp1), reduce(mult.exp2)) match {
    case (n1: Number, n2: Number) => n1 * n2
    case (BasicInteger(1), e)=> e
    case (e, BasicInteger(1)) => e
    case (m, Reciprocate(r)) => (m, r) match {
      case (a, b) if a == b => BasicInteger(1)
      case (a: Number, b: Number) => a / b
      case CommonFactor(_, a, b) => Multiply(a, Reciprocate(b))
  }
  }}

  val RecipReducer: Reducer = { case recip: Reciprocate => reduce(recip.exp) match {
    case BasicInteger(1) => BasicInteger(1)
    case other => Reciprocate(other)
  }}

  case class CommonFactor(factor: Expression, other1: Expression, other2: Expression)

  object CommonFactor {

    def unapply(tuple: (Expression, Expression)): Option[(Expression, Expression, Expression)]
      = CommonFactor.of(tuple._1, tuple._2).map(f => (f.factor, f.other1, f.other2))

    def unapply(arg: CommonFactor): Option[(Expression, Expression, Expression)] = Some((arg.factor, arg.other1, arg.other2))

    def of(e1: Expression, e2: Expression): Option[CommonFactor] = (e1, e2) match {
      case (e1: Expression, e2: Expression) if e1 == e2 => Some(CommonFactor(e1, BasicInteger(1), BasicInteger(1)))
      case (e1: Expression, Multiply(m1, m2)) => (CommonFactor.of(e1, m1), CommonFactor.of(e1, m2)) match {
        case (Some(CommonFactor(f1, a1, b1)), Some(CommonFactor(f2, a2, b2))) => Some(CommonFactor(Multiply(f1, f2), Multiply(a2, Reciprocate(f1)), Multiply(b1, b2)))
        case (Some(CommonFactor(f1, a1, b1)), None) => Some(CommonFactor(f1, a1, Multiply(b1, m2)))
        case (None, Some(CommonFactor(f1, a1, b1))) => Some(CommonFactor(f1, Multiply(a1, m1), b1))
        case _ => None
      }
      case (Multiply(m1, m2), e1: Expression) => (CommonFactor.of(e1, m1), CommonFactor.of(e1, m2)) match {
        case (Some(CommonFactor(f1, a1, b1)), Some(CommonFactor(f2, a2, b2))) => Some(CommonFactor(Multiply(f1, f2), Multiply(a2, Reciprocate(f1)), Multiply(b1, b2)))
        case (Some(CommonFactor(f1, a1, b1)), None) => Some(CommonFactor(f1, a1, Multiply(b1, m2)))
        case (None, Some(CommonFactor(f1, a1, b1))) => Some(CommonFactor(f1, Multiply(a1, m1), b1))
        case _ => None
      }
      case (b1: BasicInteger, b2: BasicInteger) =>
        val f: BasicInteger = gcf(b1, b2)
        Some(CommonFactor(f, b1 / f, b2 / f))
      case (e1, e2) => None
    }

    def gcf(n1: BasicInteger, n2: BasicInteger): BasicInteger = {
      var x1 = n1
      var x2 = n2
      while(x1 != x2) {
        if(x1 > x2) {
          x1 = (x1 - x2).asInstanceOf[BasicInteger]
        } else {
          x2 = (x2 - x1).asInstanceOf[BasicInteger]
        }
      }
      x1
    }
  }
}
