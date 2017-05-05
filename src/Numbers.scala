import ScalaMath.{Negate, Number}

object Numbers {

  case class BasicInteger(num: Long) extends Number {
    override def +(n: Number): Number = n match {
      case BasicInteger(num2) => BasicInteger(num + num2)
      case BasicDecimal(num2) => BasicDecimal(num + num2)
      case other: PowerTower => other + this
    }

    override def -(n: Number): Number = n match {
      case BasicInteger(num2) => BasicInteger(num - num2)
      case BasicDecimal(num2) => BasicDecimal(num - num2)
      case PowerTower(sign, coef, power) => PowerTower(!sign, coef, power) + this
    }

    override def *(n: Number): Number = n match {
      case BasicInteger(num2) => BasicInteger(num * num2)
      case BasicDecimal(num2) => BasicDecimal(num * num2)
      case other: PowerTower => other * this
    }

    override def /(n: Number): Number = n match {
      case BasicInteger(num2) => BasicInteger(num / num2)
      case BasicDecimal(num2) => BasicDecimal(num / num2)
      case PowerTower(sign, coef, PowerTower(powerSign, powerCoef, power)) => PowerTower(sign, coef, PowerTower(!powerSign, powerCoef, power)) * this
    }

    override def ^(n: Number): Number = n match {
      case other: BasicInteger => BasicInteger(num ^ other.num)
      case other: BasicDecimal => BasicDecimal(Math.pow(num, other.num))
      case other: PowerTower => PowerTower(num >= 0, Math.abs(num), other)
    }
  }

  case class BasicDecimal(num: Double) extends Number {
    override def +(n: Number): Number = n match {
      case other: BasicInteger => BasicDecimal(num + other.num)
      case other: BasicDecimal => BasicDecimal(num + other.num)
      case other: PowerTower => other + this
    }

    override def -(n: Number): Number = n match {
      case other: BasicInteger => BasicDecimal(num - other.num)
      case other: BasicDecimal => BasicDecimal(num - other.num)
      case PowerTower(sign, coef, power) => PowerTower(!sign, coef, power) + this
    }

    override def *(n: Number): Number = n match {
      case other: BasicInteger => BasicDecimal(num * other.num)
      case other: BasicDecimal => BasicDecimal(num * other.num)
      case other: PowerTower => other * this
    }

    override def /(n: Number): Number = n match {
      case other: BasicInteger => BasicDecimal(num / other.num)
      case other: BasicDecimal => BasicDecimal(num / other.num)
      case PowerTower(sign, coef, PowerTower(powerSign, powerCoef, power)) => PowerTower(sign, coef, PowerTower(!powerSign, powerCoef, power)) * this
    }

    override def ^(n: Number): Number = n match {
      case other: BasicInteger => BasicDecimal(Math.pow(num, other.num))
      case other: BasicDecimal => BasicDecimal(Math.pow(num, other.num))
      case other: PowerTower => PowerTower(num >= 0, Math.abs(num), other)
    }
  }

  case class PowerTower private[PowerTower](sign: Boolean, coef: Double, exp: PowerTower) extends Number {

    override def +(n: Number): Number = ???

    override def -(n: Number): Number = ???

    override def *(n: Number): Number = ???

    override def /(n: Number): Number = ???

    override def ^(n: Number): Number = ???
  }

  object PowerTower {
    def apply(sign: Boolean, coef: Double, exp: PowerTower): PowerTower = fix(new PowerTower(sign, coef, exp))

    private def fix(p: PowerTower): PowerTower = {
      ???
    }
  }

  final case object IMAGINARY extends Number {
    override def +(n: Number): Number = ???

    override def -(n: Number): Number = ???

    override def *(n: Number): Number = ???

    override def /(n: Number): Number = ???

    override def ^(n: Number): Number = ???
  }

  final val PI = BasicDecimal(Math.PI)
  final val E = BasicDecimal(Math.E)
}
