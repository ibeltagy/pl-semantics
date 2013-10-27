package utcompling.scalalogic.drt.expression

import utcompling.scalalogic.drt._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression.FolExpression
import scala.collection.mutable.ListBuffer
import utcompling.scalalogic.util.StringUtils
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerVariable
import utcompling.scalalogic.fol.expression.FolVariableExpression

case class DrtBoxExpression(refs: List[Variable], conds: List[DrtExpression], consequent: Option[DrtExpression] = None, isEqv: Option[Boolean] = Some(false))
    extends DrtExpression {

    /**
     * Replace all instances of variable v with expression E in self,
     * where v is free in self.
     */
    override def replace(variable: Variable, expression: DrtExpression, replace_bound: Boolean = false, alpha_convert: Boolean = true): DrtBoxExpression = {
        if (this.refs.contains(variable)) {
            // if a bound variable is the thing being replaced
            if (!replace_bound)
                return this
            else {
                val i = this.refs.indexOf(variable)
                val consequent = this.consequent.map(_.replace(variable, expression, true, alpha_convert))
                return DrtBoxExpression(this.refs.take(i) ++ List(expression.asInstanceOf[DrtVariableExpression].variable) ++ this.refs.drop(i + 1),
                    this.conds.map(_.replace(variable, expression, true, alpha_convert)),
                    consequent)
            }
        } else {
            var self = this
            if (alpha_convert) {
                // any bound variable that appears in the expression must
                // be alpha converted to avoid a conflict
                for (ref <- (self.refs.toSet & expression.free())) {
                    val newvar = Variable.unique(ref)
                    val newvarex = DrtVariableExpression(newvar)
                    val i = self.refs.indexOf(ref)
                    val consequent = self.consequent.map(_.replace(ref, newvarex, true, alpha_convert))
                    self = DrtBoxExpression(self.refs.take(i) ++ List(newvar) ++ self.refs.drop(i + 1),
                        self.conds.map(_.replace(ref, newvarex, true, alpha_convert)),
                        consequent)
                }
            }

            // replace in the conditions
            val consequent = self.consequent.map(_.replace(variable, expression, replace_bound, alpha_convert))
            return DrtBoxExpression(self.refs,
                self.conds.map(_.replace(variable, expression, replace_bound, alpha_convert)),
                consequent)
        }
    }

    override def free(): Set[Variable] = {
        val conds_free = this.conds.flatMap(_.free).toSet ++ this.consequent.map(_.free).flatten
        return conds_free -- this.refs.toSet
    }

    override def getRefs(recursive: Boolean = false) =
        if (recursive)
            (this.refs ++
                this.conds.map(_.getRefs(true)).flatten ++
                this.consequent.map(_.getRefs(true)).flatten).toSet
        else
            this.refs.toSet

    override def visit[S](function: DrtExpression => S, combinator: List[S] => S) =
        combinator(this.conds.map(function) ++ this.consequent.map(function))

    override def visitStructured[S](function: DrtExpression => S, combinator: List[Any] => S) =
        combinator(List(this.refs, this.conds.map(function), this.consequent.map(function)))

    override def eliminateEquality(): DrtBoxExpression = {
        var drs = this
        var i = 0
        while (i < drs.conds.length) {
            drs.conds(i) match {
                case e: DrtEqualityExpression => {
                    e.first match {
                        case f: DrtVariableExpression => {
                            e.second match {
                                case s: DrtVariableExpression => {
                                    drs = DrtBoxExpression((drs.refs.toSet - s.variable).toList,
                                        drs.conds.take(i) ++ drs.conds.drop(i + 1),
                                        drs.consequent)
                                    if (s.variable != f.variable) {
                                        drs = drs.replace(s.variable, f, false, false)
                                        i = 0
                                    }
                                    i -= 1
                                }
                            }
                        }
                    }
                }
            }
            i += 1
        }

        val conds = new ListBuffer[DrtExpression]
        for (cond <- drs.conds) {
            val new_cond = cond.eliminateEquality()
            val new_cond_simp = new_cond.simplify()
            new_cond_simp match {
                case d: DrtBoxExpression => {
                    if (d.refs.nonEmpty || d.conds.nonEmpty || d.consequent.isDefined)
                        conds += new_cond
                }
                case _ => {
                    conds += new_cond
                }
            }
        }
        return DrtBoxExpression(drs.refs, conds.result, drs.consequent.map(_.eliminateEquality))
    }

    override def fol(): FolExpression = {
        this.consequent match {
            case None => {
                if (this.conds.isEmpty)
                	//Return a variable "true" indicating that the expression is empty
                	return FolVariableExpression(Variable("true"))  
                    //throw new RuntimeException("Cannot convert DRS with no conditions to FOL: "+this)
                var accum = this.conds.map(_.fol).reduceLeft(_ & _)
                for (ref <- this._order_ref_strings(this.refs).reverse.map(Variable(_)))
                    accum = accum exists ref
                return accum
            }
            case Some(consequent) => {
                var accum = {
                    if (this.conds.nonEmpty)
                    {
                      if (!isEqv.isEmpty &  isEqv.get )
                        this.conds.map(_.fol).reduceLeft(_ & _) <-> this.consequent.get.fol()
                      else
                        this.conds.map(_.fol).reduceLeft(_ & _) -> this.consequent.get.fol()
                    }
                    else
                        this.consequent.get.fol()
                }
                for (ref <- this.refs.reverse)
                    accum = accum all ref
                return accum
            }
        }
    }

    override def _folModal(world: Variable): FolExpression = {
        this.consequent match {
            case None => {
                var accum = this.conds.map(_._folModal(world)).reduceLeft(_ & _)
                for (ref <- this._order_ref_strings(this.refs).reverse.map(Variable(_)))
                    accum = accum exists ref
                return accum
            }
            case Some(consequent) => {
                var accum = {
                    if (this.conds.nonEmpty)
                        this.conds.map(_._folModal(world)).reduceLeft(_ & _) ->
                            this.consequent.get._folModal(world)
                    else
                        this.consequent.get._folModal(world)
                }
                for (ref <- this._order_ref_strings(this.refs).reverse.map(Variable(_)))
                    accum = accum all ref
                return accum
            }
        }
    }

    override def pretty(): String = {
        val condColumn = this.conds.map(_.pretty).mkString("\n")
        val condColumnHeight = condColumn.split("\n").length
        val condColumnWidth = condColumn.split("\n").map(_.length).max

        val refLine = this._order_ref_strings(this.refs).mkString(" ")
        val refDivider = "-" * (condColumnWidth)

        val refsAndConds = List(refLine, refDivider, condColumn).mkString("\n")

        val wall = List.make((condColumnHeight + 2), "|").mkString("\n")

        val top = " _" + ("_" * condColumnWidth) + "_ "
        val bottom = "|_" + ("_" * condColumnWidth) + "_|"

        val drs = top + "\n" + StringUtils.sideBySide(wall, "\n-", refsAndConds, "\n-", wall) + "\n" + bottom

        return this.consequent.map(c => StringUtils.sideBySideCentering(DrtTokens.OPEN, drs, " ", DrtTokens.IMP, " ", c.pretty, DrtTokens.CLOSE)).getOrElse(drs)
    }

    private def _order_ref_strings(refs: List[Variable]): List[String] = {
        val strings = refs.map(_.name)
        val ind_vars = new ListBuffer[String]()
        val func_vars = new ListBuffer[String]()
        val event_vars = new ListBuffer[String]()
        val other_vars = new ListBuffer[String]()
        for (s <- strings) {
            if (Variable.isIndVar(s))
                ind_vars.append(s)
            else if (Variable.isFuncVar(s))
                func_vars.append(s)
            else if (Variable.isEventVar(s))
                event_vars.append(s)
            else
                other_vars.append(s)
        }
        return (other_vars.sorted ++
            event_vars.sortBy(s => if (s.length > 1) s.drop(1).toInt else -1) ++
            func_vars.sortBy(s => (s(0), if (s.length > 1) s.drop(1).toInt else -1)) ++
            ind_vars.sortBy(s => (s(0), if (s.length > 1) s.drop(1).toInt else -1))).result
    }

    override def toString(): String = {
        val drs = "([%s],[%s])".format(
            this._order_ref_strings(this.refs).mkString(","),
            this.conds.mkString(", "))
        if (this.consequent.isDefined)
            return DrtTokens.OPEN + drs + " " + DrtTokens.IMP + " " + this.consequent.get + DrtTokens.CLOSE
        return drs
    }

}