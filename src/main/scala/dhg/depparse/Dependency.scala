package dhg.depparse

/**
 * DEPENDENCY HIERARCHY
 */

trait Dependency {
  def value: String
  override def equals(o: Any) = o match { case d: Dependency => value == d.value; case _ => false }
  override def hashCode() = value.hashCode
  override def toString = value
}
/*    */ class Root(override val value: String) extends Dependency // root
/*    */ object Root { val Re = "(root\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Root(x)); case _ => None } }
/*        */ class Dep(override val value: String) extends Dependency // dependent
/*        */ object Dep { val Re = "(dep\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Dep(x)); case _ => None } }
/*            */ class Aux(override val value: String) extends Dep(value) // auxiliary
/*            */ object Aux { val Re = "(aux\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Aux(x)); case _ => None } }
/*                */ class AuxPass(override val value: String) extends Aux(value) // passive auxiliary
/*                */ object AuxPass { val Re = "(auxpass\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new AuxPass(x)); case _ => None } }
/*                */ class Cop(override val value: String) extends Aux(value) // copula
/*                */ object Cop { val Re = "(cop\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Cop(x)); case _ => None } }
/*            */ class Arg(override val value: String) extends Dep(value) // argument
/*            */ object Arg { val Re = "(arg\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Arg(x)); case _ => None } }
/*                */ class Agent(override val value: String) extends Arg(value) // agent
/*                */ object Agent { val Re = "(agent\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Agent(x)); case _ => None } }
/*                */ class Comp(override val value: String) extends Arg(value) // complement
/*                */ object Comp { val Re = "(comp\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Comp(x)); case _ => None } }
/*                    */ class AComp(override val value: String) extends Comp(value) // adjectival complement
/*                    */ object AComp { val Re = "(acomp\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new AComp(x)); case _ => None } }
/*                    */ class Attr(override val value: String) extends Comp(value) // attributive
/*                    */ object Attr { val Re = "(attr\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Attr(x)); case _ => None } }
/*                    */ class CComp(override val value: String) extends Comp(value) // clausal complement with internal subject
/*                    */ object CComp { val Re = "(ccomp\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new CComp(x)); case _ => None } }
/*                    */ class PComp(override val value: String) extends Comp(value) // prepositional complement
/*                    */ object PComp { val Re = "(pcomp\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new PComp(x)); case _ => None } }
/*                    */ class XComp(override val value: String) extends Comp(value) // clausal complement with external subject
/*                    */ object XComp { val Re = "(xcomp\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new XComp(x)); case _ => None } }
/*                    */ class Complm(override val value: String) extends Comp(value) // complementizer
/*                    */ object Complm { val Re = "(complm\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Complm(x)); case _ => None } }
/*                    */ class Obj(override val value: String) extends Comp(value) // object
/*                    */ object Obj { val Re = "(obj\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Obj(x)); case _ => None } }
/*                        */ class DObj(override val value: String) extends Obj(value) // direct object
/*                        */ object DObj { val Re = "(dobj\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new DObj(x)); case _ => None } }
/*                        */ class IObj(override val value: String) extends Obj(value) // indirect object
/*                        */ object IObj { val Re = "(iobj\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new IObj(x)); case _ => None } }
/*                        */ class PObj(override val value: String) extends Obj(value) // object of preposition
/*                        */ object PObj { val Re = "(pobj\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new PObj(x)); case _ => None } }
/*                    */ class Mark(override val value: String) extends Comp(value) // marker (word introducing an advcl)
/*                    */ object Mark { val Re = "(mark\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Mark(x)); case _ => None } }
/*                    */ class Rel(override val value: String) extends Comp(value) // relative (word introducing a rcmod)
/*                    */ object Rel { val Re = "(rel\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Rel(x)); case _ => None } }
/*                */ class Subj(override val value: String) extends Arg(value) // subject
/*                */ object Subj { val Re = "(subj\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Subj(x)); case _ => None } }
/*                    */ class NSubj(override val value: String) extends Subj(value) // nominal subject
/*                    */ object NSubj { val Re = "(nsubj\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new NSubj(x)); case _ => None } }
/*                        */ class NSubjPass(override val value: String) extends NSubj(value) // passive nominal subject
/*                        */ object NSubjPass { val Re = "(nsubjpass\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new NSubjPass(x)); case _ => None } }
/*                    */ class CSubj(override val value: String) extends Subj(value) // clausal subject
/*                    */ object CSubj { val Re = "(csubj\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new CSubj(x)); case _ => None } }
/*                        */ class CSubjPass(override val value: String) extends CSubj(value) // passive clausal subject
/*                        */ object CSubjPass { val Re = "(csubjpass\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new CSubjPass(x)); case _ => None } }
/*            */ class CC(override val value: String) extends Dep(value) // coordination
/*            */ object CC { val Re = "(cc\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new CC(x)); case _ => None } }
/*            */ class Conj(override val value: String) extends Dep(value) // conjunct
/*            */ object Conj { val Re = "(conj\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Conj(x)); case _ => None } }
/*            */ class Expl(override val value: String) extends Dep(value) // expletive (expletive \there")
/*            */ object Expl { val Re = "(expl\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Expl(x)); case _ => None } }
/*            */ class Mod(override val value: String) extends Dep(value) // modifier
/*            */ object Mod { val Re = "(mod\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Mod(x)); case _ => None } }
/*                */ class Abbrev(override val value: String) extends Mod(value) // abbreviation modifier
/*                */ object Abbrev { val Re = "(abbrev\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Abbrev(x)); case _ => None } }
/*                */ class AMod(override val value: String) extends Mod(value) // adjectival modifier
/*                */ object AMod { val Re = "(amod\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new AMod(x)); case _ => None } }
/*                */ class Appos(override val value: String) extends Mod(value) // appositional modifier
/*                */ object Appos { val Re = "(appos\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Appos(x)); case _ => None } }
/*                */ class AdvCl(override val value: String) extends Mod(value) // adverbial clause modifier
/*                */ object AdvCl { val Re = "(advcl\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new AdvCl(x)); case _ => None } }
/*                */ class PurpCl(override val value: String) extends Mod(value) // purpose clause modifier
/*                */ object PurpCl { val Re = "(purpcl\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new PurpCl(x)); case _ => None } }
/*                */ class Det(override val value: String) extends Mod(value) // determiner
/*                */ object Det { val Re = "(det\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Det(x)); case _ => None } }
/*                */ class PreDet(override val value: String) extends Mod(value) // predeterminer
/*                */ object PreDet { val Re = "(predet\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new PreDet(x)); case _ => None } }
/*                */ class PreConj(override val value: String) extends Mod(value) // preconjunct
/*                */ object PreConj { val Re = "(preconj\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new PreConj(x)); case _ => None } }
/*                */ class Infmod(override val value: String) extends Mod(value) // infinitival modifier
/*                */ object Infmod { val Re = "(infmod\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Infmod(x)); case _ => None } }
/*                */ class MWE(override val value: String) extends Mod(value) // multi-word expression modifier
/*                */ object MWE { val Re = "(mwe\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new MWE(x)); case _ => None } }
/*                */ class PartMod(override val value: String) extends Mod(value) // participial modifier
/*                */ object PartMod { val Re = "(partmod\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new PartMod(x)); case _ => None } }
/*                */ class AdvMod(override val value: String) extends Mod(value) // adverbial modifier
/*                */ object AdvMod { val Re = "(advmod\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new AdvMod(x)); case _ => None } }
/*                    */ class Neg(override val value: String) extends AdvMod(value) // negation modifier
/*                    */ object Neg { val Re = "(neg\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Neg(x)); case _ => None } }
/*                */ class RcMod(override val value: String) extends Mod(value) // relative clause modifier
/*                */ object RcMod { val Re = "(rcmod\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new RcMod(x)); case _ => None } }
/*                */ class QuantMod(override val value: String) extends Mod(value) // quantifier modifier
/*                */ object QuantMod { val Re = "(quantmod\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new QuantMod(x)); case _ => None } }
/*                */ class NN(override val value: String) extends Mod(value) // noun compound modifier
/*                */ object NN { val Re = "(nn\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new NN(x)); case _ => None } }
/*                */ class NpAdvMod(override val value: String) extends Mod(value) // noun phrase adverbial modifier
/*                */ object NpAdvMod { val Re = "(npadvmod\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new NpAdvMod(x)); case _ => None } }
/*                    */ class TMod(override val value: String) extends NpAdvMod(value) // temporal modifier
/*                    */ object TMod { val Re = "(tmod\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new TMod(x)); case _ => None } }
/*                */ class Num(override val value: String) extends Mod(value) // numeric modifier
/*                */ object Num { val Re = "(num\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Num(x)); case _ => None } }
/*                */ class Number(override val value: String) extends Mod(value) // element of compound number
/*                */ object Number { val Re = "(number\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Number(x)); case _ => None } }
/*                */ class Prep(override val value: String) extends Mod(value) // prepositional modifier
/*                */ object Prep { val Re = "(prep\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Prep(x)); case _ => None } }
/*                */ class Poss(override val value: String) extends Mod(value) // possession modifier
/*                */ object Poss { val Re = "(poss\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Poss(x)); case _ => None } }
/*                */ class Possessive(override val value: String) extends Mod(value) // possessive modifier ('s)
/*                */ object Possessive { val Re = "(possessive\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Possessive(x)); case _ => None } }
/*                */ class Prt(override val value: String) extends Mod(value) // phrasal verb particle
/*                */ object Prt { val Re = "(prt\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Prt(x)); case _ => None } }
/*            */ class Parataxis(override val value: String) extends Dep(value) // parataxis
/*            */ object Parataxis { val Re = "(parataxis\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Parataxis(x)); case _ => None } }
/*            */ class Punct(override val value: String) extends Dep(value) // punctuation
/*            */ object Punct { val Re = "(punct\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Punct(x)); case _ => None } }
/*            */ class Ref(override val value: String) extends Dep(value) // referent
/*            */ object Ref { val Re = "(ref\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new Ref(x)); case _ => None } }
/*            */ class SDep(override val value: String) extends Dep(value) // semantic dependent
/*            */ object SDep { val Re = "(sdep\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new SDep(x)); case _ => None } }
/*                */ class XSubj(override val value: String) extends SDep(value) // controlling subject
/*                */ object XSubj { val Re = "(xsubj\\S*)".r; def unapply(r: Any) = r.toString match { case Re(x) => Some(new XSubj(x)); case _ => None } }

object Dependency {
  def apply(s: String) = s.toLowerCase match {
    case Root(x) => x
    case Dep(x) => x
    case AuxPass(x) => x
    case Aux(x) => x
    case Cop(x) => x
    case Arg(x) => x
    case Agent(x) => x
    case Comp(x) => x
    case AComp(x) => x
    case Attr(x) => x
    case CComp(x) => x
    case PComp(x) => x
    case XComp(x) => x
    case Complm(x) => x
    case Obj(x) => x
    case DObj(x) => x
    case IObj(x) => x
    case PObj(x) => x
    case Mark(x) => x
    case Rel(x) => x
    case Subj(x) => x
    case NSubjPass(x) => x
    case NSubj(x) => x
    case CSubjPass(x) => x
    case CSubj(x) => x
    case CC(x) => x
    case Conj(x) => x
    case Expl(x) => x
    case Mod(x) => x
    case Abbrev(x) => x
    case AMod(x) => x
    case Appos(x) => x
    case AdvCl(x) => x
    case PurpCl(x) => x
    case Det(x) => x
    case PreDet(x) => x
    case PreConj(x) => x
    case Infmod(x) => x
    case MWE(x) => x
    case PartMod(x) => x
    case AdvMod(x) => x
    case Neg(x) => x
    case RcMod(x) => x
    case QuantMod(x) => x
    case NN(x) => x
    case NpAdvMod(x) => x
    case TMod(x) => x
    case Number(x) => x
    case Num(x) => x
    case Prep(x) => x
    case Possessive(x) => x
    case Poss(x) => x
    case Prt(x) => x
    case Parataxis(x) => x
    case Punct(x) => x
    case Ref(x) => x
    case SDep(x) => x
    case XSubj(x) => x
  }
} 
