import ScalaMath._

object AlgebraReducers {

  val AddReducer: Reducer = Reducer[Add] {
     case Add(n1: Number, n2: Number) => n1 + n2
  }

  val MultReducer: Reducer = Reducer[Multiply] {
    case Multiply(n1: Number, n2: Number) => n1 * n2
  }
}
