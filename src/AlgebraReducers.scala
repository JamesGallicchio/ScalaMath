import ScalaMath._

object AlgebraReducers {

   val addReducer: Reducer = {
     case Add(n1: Number, n2: Number) => n1 + n2
   }

  val multReducer: Reducer = {
    case Multiply(n1: Number, n2: Number) => n1 * n2
  }
}
