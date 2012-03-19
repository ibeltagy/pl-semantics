package utcompling.mlnsemantics.flat

import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.fol.expression.FolAtom
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.util.Counter

object PredAtom {
  val PRED = "pred_"
  def apply(prop: String, cur: String, name: String, variable: String) =
    FolAtom(Variable(PRED), Variable(prop), Variable(cur), Variable(name), Variable(variable))
  def unapply(ae: FolExpression) = ae match {
    case FolAtom(Variable(PRED), Variable(prop), Variable(cur), Variable(name), Variable(variable)) => Some(prop, cur, name, variable)
    case _ => None
  }
}

object NamedAtom {
  val PRED = "named_"
  def apply(prop: String, cur: String, name: String, variable: String) =
    FolAtom(Variable(PRED), Variable(prop), Variable(cur), Variable(name), Variable(variable))
  def unapply(ae: FolExpression) = ae match {
    case FolAtom(Variable(PRED), Variable(prop), Variable(cur), Variable(name), Variable(variable)) => Some(prop, cur, name, variable)
    case _ => None
  }
}

object RelAtom {
  val PRED = "rel_"
  def apply(prop: String, cur: String, name: String, event: String, variable: String) =
    FolAtom(Variable(PRED), Variable(prop), Variable(cur), Variable(name), Variable(event), Variable(variable))
  def unapply(ae: FolExpression) = ae match {
    case FolAtom(Variable(PRED), Variable(prop), Variable(cur), Variable(name), Variable(event), Variable(variable)) =>
      Some(prop, cur, name, event, variable)
    case _ => None
  }
}

object EqAtom {
  val PRED = "eq_"
  def apply(prop: String, cur: String, first: String, second: String) =
    FolAtom(Variable(PRED), Variable(prop), Variable(cur), Variable(first), Variable(second))
  def unapply(ae: FolExpression) = ae match {
    case FolAtom(Variable(PRED), Variable(prop), Variable(cur), Variable(first), Variable(second)) =>
      Some(prop, cur, first, second)
    case _ => None
  }

  // true(p2) -> (x0 = x3)
  //    def apply(prop: String, cur: String, first: String, second: String) = FolIfExpression(TrueAtom(prop), FolEqualityExpression(FolAtom(Variable(first)),FolAtom(Variable(second))))
  //    def unapply(ae: FolExpression) = ae match {
  //        case FolIfExpression(TrueAtom(prop), FolEqualityExpression(FolAtom(Variable(first)),FolAtom(Variable(second)))) => Some(prop, first, second)
  //        case _ => None
  //    }

  // true(p2) -> all p.(outscopes(p,p2) -> all n.(pred_(p, n, x0) <-> pred_(p, n, x3))
  //    def apply(prop: String, first: String, second: String) =
  //        FolIfExpression(TrueAtom(prop),
  //            FolAllExpression(Variable("p"), FolIfExpression(OutscopesAtom(prop, "p"),
  //                FolAllExpression(Variable("n"), FolIffExpression(PredAtom("p", "c", None, "n", first), PredAtom("p", "c", None, "n", second))))))
  //    def unapply(ae: FolExpression) = ae match {
  //        case FolIfExpression(TrueAtom(prop1),
  //            FolAllExpression(Variable("p"), FolIfExpression(OutscopesAtom(prop2, "p"),
  //                FolAllExpression(Variable("n"), FolIffExpression(PredAtom("p", "c", None, "n", first), PredAtom("p", "c", None, "n", second)))))) if (prop1 == prop2) =>
  //            Some(prop1, first, second)
  //        case _ => None
  //    }
}

object PredExistAtom {
  val PRED = "pred_exist_"
  def apply(cur: String, name: String, variable: String) =
    FolAtom(Variable(PRED), Variable(cur), Variable(name), Variable(variable))
  def unapply(ae: FolExpression) = ae match {
    case FolAtom(Variable(PRED), Variable(cur), Variable(name), Variable(variable)) => Some(cur, name, variable)
    case _ => None
  }
}

object NamedExistAtom {
  val PRED = "named_exist_"
  def apply(cur: String, name: String, variable: String) =
    FolAtom(Variable(PRED), Variable(cur), Variable(name), Variable(variable))
  def unapply(ae: FolExpression) = ae match {
    case FolAtom(Variable(PRED), Variable(cur), Variable(name), Variable(variable)) => Some(cur, name, variable)
    case _ => None
  }
}

object RelExistAtom {
  val PRED = "rel_exist_"
  def apply(cur: String, name: String, event: String, variable: String) =
    FolAtom(Variable(PRED), Variable(cur), Variable(name), Variable(event), Variable(variable))
  def unapply(ae: FolExpression) = ae match {
    case FolAtom(Variable(PRED), Variable(cur), Variable(name), Variable(event), Variable(variable)) => Some(cur, name, event, variable)
    case _ => None
  }
}

object EqExistAtom {
  val PRED = "eq_exist_"
  def apply(cur: String, first: String, second: String) =
    FolAtom(Variable(PRED), Variable(cur), Variable(first), Variable(second))
  def unapply(ae: FolExpression) = ae match {
    case FolAtom(Variable(PRED), Variable(cur), Variable(first), Variable(second)) => Some(cur, first, second)
    case _ => None
  }
}

object TrueAtom {
  val PRED = "true"
  def apply(prop: String) =
    FolAtom(Variable(PRED), Variable(prop))
  def unapply(ae: FolExpression) = ae match {
    case FolAtom(Variable(PRED), Variable(prop)) => Some(prop)
    case _ => None
  }
}

object OutscopesAtom {
  val PRED = "outscopes"
  def apply(prop1: String, prop2: String) = FolAtom(Variable(PRED), Variable(prop1), Variable(prop2))
  def unapply(ae: FolExpression) = ae match {
    case FolAtom(Variable(PRED), Variable(prop1), Variable(prop2)) => Some(prop1, prop2)
    case _ => None
  }
}

object NotAtom {
  val PRED = "not_"
  def apply(prop1: String, prop2: String) = FolAtom(Variable(PRED), Variable(prop1), Variable(prop2))
  def unapply(ae: FolExpression) = ae match {
    case FolAtom(Variable(PRED), Variable(prop1), Variable(prop2)) => Some(prop1, prop2)
    case _ => None
  }
}

object PropAtom {
  val PRED = "prop_"
  def apply(prop1: String, prop2: String) = FolAtom(Variable(PRED), Variable(prop1), Variable(prop2))
  def unapply(ae: FolExpression) = ae match {
    case FolAtom(Variable(PRED), Variable(prop1), Variable(prop2)) => Some(prop1, prop2)
    case _ => None
  }
}

object ImpAtom {
  val PRED = "imp_"
  def apply(prop: String, aProp: String, cProp: String) = FolAtom(Variable(PRED), Variable(prop), Variable(aProp), Variable(cProp))
  def unapply(ae: FolExpression) = ae match {
    case FolAtom(Variable(PRED), Variable(prop), Variable(aProp), Variable(cProp)) => Some(prop, aProp, cProp)
    case _ => None
  }
}

