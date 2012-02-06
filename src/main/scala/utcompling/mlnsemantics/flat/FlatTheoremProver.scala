package utcompling.mlnsemantics.flat

import utcompling.scalalogic.inference.TheoremProver
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression.parse.FolLogicParser
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SetBuilder
import scala.collection.mutable.HashMap
import utcompling.scalalogic.util.StringUtils
import utcompling.scalalogic.util.Counter

class FlatTheoremProver[R](
    theoremProver: TheoremProver[FolExpression, R],
    folExpressionInterpreters: List[FolExpressionInterpreter]) {

    private val p = new FolLogicParser().parse(_)

    private val VAR_RE = """^"([pex])\d*"$""".r
    private val NAME_RE = """^"(.+)"$""".r
    private val TRUTH_RE = """^"(true|false)"$""".r
    private val PRED_RE = """^pred_([pex])_$""".r
    private val NAMED_RE = """^named_([pex])_$""".r
    private val REL_RE = """^rel_([pex])_([pex])_$""".r
    private val EQ_RE = """^eq_([ex])_$""".r
    private val PRED_EXIST_RE = """^pred_exist_([pex])_$""".r
    private val NAMED_EXIST_RE = """^named_exist_([pex])_$""".r
    private val REL_EXIST_RE = """^rel_exist_([pex])_([pex])_$""".r
    private val EQ_EXIST_RE = """^eq_exist_([ex])_$""".r

    def spill(assumptions: List[FolExpression], goal: FolExpression, preds: List[String], verbose: Boolean = false) = {
        val newAssumptions = assumptions.map(f)
        val newGoal = f(goal)

        val proverAssumptions = newAssumptions ++ getRules(newAssumptions, List(newGoal))

        def canProve(goal: FolExpression) = theoremProver.prove(proverAssumptions, goal, verbose).isDefined

        if (canProve(p("P&-P"))) {
            println("INCONSISTENT")
        } else {
            val atoms = getAtoms(proverAssumptions.reduceLeft(_ & _) & newGoal)

            val varsBuilder = HashMap() ++ List("p", "e", "x").map(_ -> new SetBuilder[Variable, Set[Variable]](Set[Variable]()))
            for (FolAtom(Variable(pred), args @ _*) <- atoms) {
                for (arg <- args) {
                    arg.name match {
                        case VAR_RE(t) => {
                            val s = varsBuilder(t)
                            s += arg
                        }
                        case NAME_RE(t) => {
                            val s = varsBuilder.getOrElseUpdate(pred, new SetBuilder[Variable, Set[Variable]](Set[Variable]()))
                            s += arg
                        }
                        case _ => {

                        }
                    }
                }
            }
            val vars = varsBuilder.mapValues(_.result)

            val cVars =
                if (false)
                    vars("p")
                else
                    Set(Variable(""""p1""""))

            val newAtoms = atoms.flatMap {
                case FolAtom(pred, args @ _*) =>
                    pred.name match {
                        case PRED_RE(a) => for (p <- vars("p"); c <- cVars; n <- vars(pred.name); a <- vars(a)) yield FolAtom(pred, p, c, n, a)
                        case NAMED_RE(a) => for (p <- vars("p"); c <- cVars; n <- vars(pred.name); a <- vars(a)) yield FolAtom(pred, p, c, n, a)
                        case REL_RE(a, b) => for (p <- vars("p"); c <- cVars; n <- vars(pred.name); a <- vars(a); b <- vars(b)) yield FolAtom(pred, p, c, n, a, b)
                        case EQ_RE(a) => for (p <- vars("p"); c <- cVars; x <- vars(a); y <- vars(a)) yield FolAtom(pred, p, c, x, y)
                        case "true" => for (p <- vars("p")) yield FolAtom(pred, p)
                        case "outscopes" => for (p <- vars("p"); c <- vars("p")) yield FolAtom(pred, p, c)
                        case _ => Set[FolApplicationExpression]()
                    }
            }.toSet

            val useAtoms = preds.flatMap(v => newAtoms.filter { case FolAtom(Variable(pred), args @ _*) => pred.startsWith(v) }.toList.sortBy(_.toString))
            for (x <- useAtoms) {
                if (canProve(x)) {
                    println(" " + x)
                }
                if (canProve(-x)) {
                    println(-x)
                }
            }
        }
    }

    def prove(assumptions: List[FolExpression], goal: FolExpression, verbose: Boolean = false): Option[R] = {
        val newAssumptions = assumptions.map(f)
        val newGoal = f(goal)

        val useAssumptions = newAssumptions ++ getRules(newAssumptions, List(newGoal))

        if (theoremProver.prove(useAssumptions, p("P&-P"), verbose).isDefined) {
            throw new RuntimeException("INCONSISTENT")
        }

        theoremProver.prove(useAssumptions, newGoal, verbose)
    }

    def satisfiable(assumptions: List[FolExpression]): Boolean = {
        val newAssumptions = assumptions.map(f)
        theoremProver.prove(newAssumptions ++ getRules(newAssumptions, List()), None).isEmpty
    }

    private def f(e: FolExpression) =
        folExpressionInterpreters.map(_.interpret _).reduceLeft(_ andThen _)(e)

    def proveVisualize(assumptions: List[FolExpression], goal: FolExpression, verbose: Boolean = false): (Option[R], String) = {
        var msg = ""
        var proof: Option[R] = None
        try {
            proof = this.prove(assumptions, goal, verbose)
            msg = proof.isDefined.toString
        } catch {
            case e => msg = e.getMessage
        }
        val visual = StringUtils.sideBySideCentering(StringUtils.box(assumptions.map(_.pretty).mkString("\n")), " + ",
            StringUtils.box(getRules(assumptions, List(goal)).mkString("\n")), " = " + msg + " => ",
            StringUtils.box(goal.pretty))
        return (proof, visual)
    }

    private def getRules(assumptions: List[FolExpression], goal: List[FolExpression]): List[FolExpression] = {
        val stuff = (assumptions ++ goal).flatMap(_.predicates).map(_.name).groupBy(_.split("_")(0)).mapValues(_.toSet).filterKeys(List("pred", "named", "rel", "eq").contains)
        val allPredicates = stuff.mapValues(_.map {
            case PRED_RE(a) => (List(a), a + "_", "x", "x")
            case NAMED_RE(a) => (List(a), a + "_", "x", "x")
            case REL_RE(a, b) => (List(a, b), a + "_" + b + "_", "x y", "x,y")
            case EQ_RE(a) => (List(a), a + "_", "x", "x")
            case "pred_" | "named_" | "eq_" => (List(), "", "x", "x")
            case "rel_" => (List(), "", "x y", "x,y")
            case "pred_exist_" | "named_exist_" | "eq_exist_" => (List(), "", "x", "x")
            case "rel_exist_" => (List(), "", "x y", "x,y")
            case x => x match {
                case PRED_EXIST_RE(a) => (List(a), a + "_", "x", "x")
                case NAMED_EXIST_RE(a) => (List(a), a + "_", "x", "x")
                case REL_EXIST_RE(a, b) => (List(a, b), a + "_" + b + "_", "x y", "x,y")
                case EQ_EXIST_RE(a) => (List(a), a + "_", "x", "x")
            }
        })

        val rules = new ListBuffer[FolExpression]

        //rules += p("""all p.((true(p) | -true(p)) & (-true(p) | --true(p)))""")
        //rules += p("""all p t1 t2.((truth(p, t1) & truth(p, t2)) -> (t1==t2))""")

        rules += p("""all p.(true(p)  -> all c.(not_(p,c) -> -true(c)))""")
        rules += p("""all p.(-true(p) -> all c.(not_(p,c) -> true(c)))""")

        rules += p("""all p.(true(p)  -> all a c.(imp_(p,a,c) -> (-true(a) | true(c))))""")
        rules += p("""all p.(-true(p) -> all a c.(imp_(p,a,c) -> (true(a)  & -true(c))))""")

        rules += p("""all p.(true(p)  -> all a c.(or_(p,a,c) -> (true(a) | true(c))))""")
        rules += p("""all p.(-true(p) -> all a c.(or_(p,a,c) -> (-true(a) & -true(c))))""")

        rules += p("""all p.(true(p)  -> all c.(prop_(p,c) -> true(c)))""") //TODO: if no theme!
        rules += p("""all p.(-true(p) -> all c.(prop_(p,c) -> -true(c)))""") //TODO: if no theme!

        //        for ((pred, args) <- allAtoms.map(_.uncurry).map { case (p, a) => (p.asInstanceOf[FolVariableExpression].variable, a.map(_.asInstanceOf[FolVariableExpression].variable)) }) {
        //            if (pred.name == "not_")
        //                rules += OutscopesAtom(args: _*)
        //            else if (pred.name == "prop_")
        //                rules += OutscopesAtom(args: _*)
        //            else if (pred.name == "imp_") {
        //                val List(p, a, c) = args
        //                rules ++= List((p, a), (p, c), (a, c)).map { case (x, y) => OutscopesAtom(x, y) }
        //            }
        //        }

        rules += p("""all p c.(not_(p,c) -> outscopes(p,c))""")
        rules += p("""all p v c.(prop_(p,c) -> outscopes(p,c))""")
        rules += p("""all p a c.(imp_(p,a,c) -> (outscopes(p,a) & outscopes(p,c) & outscopes(a,c)))""")

        rules += p("""all p.outscopes(p,p)""") //reflexive
        rules += p("""all p1 p2 p3.(((outscopes(p1,p2) & outscopes(p2,p3)) -> outscopes(p1,p3)))""") //transitive

        // Atoms are valid in subordinate DRSs 
        //        for ((pred, preds) <- allPredicates)
        //            for ((_, predTypes, quants, args) <- preds)
        //                rules += p("""all c1 c2.(outscopes(c1,c2) -> all p n %s.(%s_%s(p,c1,n,%s) -> %s_%s(p,c2,n,%s)))""".format(quants, pred, predTypes, args, pred, predTypes, args))

        // Atoms bubble up through the hierarchy
        for ((pred, preds) <- allPredicates.filterKeys(List("pred", "named", "rel", "eq").contains))
            for ((_, predTypes, quants, args) <- preds) {
                rules += p("""all c1 c2.(outscopes(c1,c2) -> (((true(c1) & true(c2))  | (-true(c1) & -true(c2))) -> all p n %s.( %s_%s(p,c2,n,%s) ->  %s_%s(p,c1,n,%s))))""".format(quants, pred, predTypes, args, pred, predTypes, args))
                rules += p("""all c1 c2.(outscopes(c1,c2) -> (((true(c1) & true(c2))  | (-true(c1) & -true(c2))) -> all p n %s.(-%s_%s(p,c2,n,%s) -> -%s_%s(p,c1,n,%s))))""".format(quants, pred, predTypes, args, pred, predTypes, args))
                rules += p("""all c1 c2.(outscopes(c1,c2) -> (((true(c1) & -true(c2)) | (-true(c1) & true(c2)))  -> all p n %s.( %s_%s(p,c2,n,%s) -> -%s_%s(p,c1,n,%s))))""".format(quants, pred, predTypes, args, pred, predTypes, args))
                rules += p("""all c1 c2.(outscopes(c1,c2) -> (((true(c1) & -true(c2)) | (-true(c1) & true(c2)))  -> all p n %s.(-%s_%s(p,c2,n,%s) ->  %s_%s(p,c1,n,%s))))""".format(quants, pred, predTypes, args, pred, predTypes, args))
            }

        // Every predicate in a DRS has the truth value of the DRS
        //        for ((pred, preds) <- allPredicates)
        //            for ((_, predTypes, quants, args) <- preds)
        //                rules += p("""all p c n %s.(true(p) <-> %s_%s(p,c,n,%s))""".format(quants, pred, predTypes, args))

        // Equality
        for ((eqType, eqPredTypes, _, _) <- allPredicates.getOrElse("eq", Set())) {
            for ((pred, preds) <- allPredicates.filterKeys(List("pred", "named", "eq").contains))
                for ((typ, predTypes, _, _) <- preds)
                    if (eqType == typ)
                        rules += p("""all p c x y.(eq_%s(p,c,x,y) -> all p2 n.(%s_%s(p2,c,n,x) <-> %s_%s(p2,c,n,y)))""".format(eqPredTypes, pred, predTypes, pred, predTypes))
            for ((pred, preds) <- allPredicates.filterKeys(List("rel").contains))
                for ((types, predTypes, _, _) <- preds) {
                    if (eqType.isEmpty || eqType(0) == types(0))
                        rules += p("""all p c x y.(eq_%s(p,c,x,y) -> all p2 n a.(%s_%s(p2,c,n,x,a) <-> %s_%s(p2,c,n,y,a)))""".format(eqPredTypes, pred, predTypes, pred, predTypes))
                    if (eqType.isEmpty || eqType(0) == types(1))
                        rules += p("""all p c x y.(eq_%s(p,c,x,y) -> all p2 n a.(%s_%s(p2,c,n,a,x) <-> %s_%s(p2,c,n,a,y)))""".format(eqPredTypes, pred, predTypes, pred, predTypes))
                }
        }

        //        for(pred <- allPredicates("pred"))
        //            rules += p("""all p.(true(p) ->  all p1 p2 x.((%s_hypernym(p1,p2) & %s(p,p1,x)) -> %s(p,p2,x)))""" % (pred,pred,pred))
        //            rules += p("""all p.(-true(p) -> all p1 p2 x.((%s_hypernym(p1,p2) & %s(p,p2,x)) -> %s(p,p1,x)))""" % (pred,pred,pred))

        for (r <- rules.result) //TODO: REMOVE
            require(r.free.isEmpty, "Rule '" + r + "' contains a free variable")

        for ((pred, preds) <- allPredicates)
            for ((_, predTypes, quants, args) <- preds) {
                rules += p("all p c n %s.(%s_%s(p,c,n,%s) -> %s_exist_%s(c,n,%s))".format(quants, pred, predTypes, args, pred, predTypes, args))
            }

        return rules.result
    }

    private def getAtoms(e: FolExpression): List[FolApplicationExpression] =
        e match {
            case ae @ FolAtom(_, _*) => List(ae)
            case ve: FolVariableExpression => List()
            case _ => e.visit(getAtoms, (parts: List[List[FolApplicationExpression]]) => parts.flatten)
        }

}
