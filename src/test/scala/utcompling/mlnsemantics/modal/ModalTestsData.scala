package utcompling.mlnsemantics.modal

import utcompling.scalalogic.discourse.impl.BoxerDiscourseInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.PassthroughBoxerExpressionInterpreter
import utcompling.scalalogic.discourse.candc.call.impl.BoxerImpl
import utcompling.scalalogic.discourse.candc.call.impl.CandcImpl
import utcompling.scalalogic.discourse.impl.CandcDiscourseParser
import utcompling.scalalogic.discourse.candc.parse.output.impl._
import utcompling.scalalogic.discourse.DiscourseParser
import utcompling.scalalogic.discourse.DiscourseInterpreter
import utcompling.scalalogic.discourse.candc.boxer.expression.parse.BoxerExpressionParser
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.MergingBoxerExpressionInterpreterDecorator
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl.Boxer2DrtExpressionInterpreter
import opennlp.scalabha.util.FileUtils
import org.junit.Test
import utcompling.mlnsemantics.modal.ModalTestsData._

object ModalTestsData {

  //
  // Sentences
  //
  val johnForgotToLeave = "John forgot to leave ."
  val johnDidNotForgetToLeave = "John did not forget to leave ."
  val johnLeft = "John left ."
  val johnDidntLeave = "John did not leave ."
  val someoneLeft = "Someone left ."
  val someoneDidntLeft = "Someone did not leave ."

  val everyDogWalks = "Every dog walks ."
  val fidoIsADogAndFidoWalks = "Fido is a dog and every dog walks ."
  val fidoIsADogAndFidoDidNotWalk = "Fido is a dog and fido does not walk ."
  val aDogWalks = "A dog walks ."
  val aDogDidNotWalk = "A dog did not walk ."
  val ifRoverIsADogThenRoverWalks = "If Rover is a dog then Rover walks ."

  val johnDidNotManageToLeave = "John did not manage to leave ."
  val johnDidNotManageToNotForgetToLeave = "John did not manage to not forget to leave ."
  val johnPretendedToLeave = "John pretended to leave ."
  val johnDeclinedToLeave = "John declined to leave ."
  val johnDidNotDeclineToLeave = "John did not decline to leave ."
  val maryForgotThatJohnLeft = "Mary forgot that John left ."
  val johnHesitatedToLeave = "John hesitated to leave ."
  val johnDidNotHesitateToLeave = "John did not hesitate to leave ."
  val edForgotToLockTheDoor = "Ed forgot to lock the door ."
  val edForgotThatHeLockedTheDoor = "Ed forgot that he locked the door ."
  val edForgotHeLockedTheDoor = "Ed forgot he locked the door ."
  val edLockedTheDoor = "Ed locked the door ."
  val edDidNotLockTheDoor = "Ed did not lock the door ."

  //fact_pp
  val maryKnewThatJohnLeft = "Mary knew that John left ."
  val maryDidNotKnowThatJohnLeft = "Mary did not know that John left ."
  val maryKnewThatJohnDidNotLeave = "Mary knew that John did not leave ."
  val maryDidNotKnowThatJohnDidNotLeave = "Mary did not know that John did not leave ."

  val johnBoughtAConvertible = "John bought a convertible ."
  val johnDidNotBuyAConvertible = "John did not buy a convertible ."
  val johnBoughtACar = "John bought a car ."
  val johnDidNotBuyACar = "John did not buy a car ."
  val johnBoughtAnAutomobile = "John bought an automobile ."
  val johnDidNotBuyAnAutomobile = "John did not buy an automobile ."

  //
  // DRSs
  //
  val johnForgotToLeave_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([[1002]:e1,[]:p2],[[1002]:pred(e1,forget,v,0),[1002]:rel(e1,x0,agent,0),[1002]:rel(e1,p2,theme,0),[1002]:prop(p2,drs([[1004]:e3],[[1004]:pred(e3,leave,v,0),[1004]:rel(e3,x0,agent,0)]))]))"
  val johnDidNotForgetToLeave_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([],[[1003]:not(drs([[1004]:e1,[]:p2],[[1004]:pred(e1,forget,v,0),[1004]:rel(e1,x0,agent,0),[1004]:rel(e1,p2,theme,0),[1004]:prop(p2,drs([[1006]:e3],[[1006]:pred(e3,leave,v,0),[1006]:rel(e3,x0,agent,0)]))]))]))"
  val johnLeft_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([[1002]:e1],[[1002]:pred(e1,leave,v,0),[1002]:rel(e1,x0,agent,0)]))"
  val johnDidntLeave_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([],[[1003]:not(drs([[1004]:e1],[[1004]:pred(e1,leave,v,0),[1004]:rel(e1,x0,agent,0)]))]))"
  val someoneLeft_drs = "drs([[1001]:x0,[1002]:e1],[[1002]:pred(e1,leave,v,0),[1002]:rel(e1,x0,agent,0)])"
  val someoneDidntLeft_drs = "drs([[1001]:x0],[[1003]:not(drs([[1004]:e1],[[1004]:pred(e1,leave,v,0),[1004]:rel(e1,x0,agent,0)]))])"

  val everyDogWalks_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,fido,per,0)]),smerge(drs([[1002]:p1],[[1002]:prop(p1,drs([[1003]:x2],[[1004]:pred(x2,dog,n,0),[1002]:eq(x0,x2)]))]),drs([],[[2001]:imp(drs([[2001]:x3],[[2002]:pred(x3,dog,n,0)]),drs([[2003]:e4],[[2003]:pred(e4,walk,v,0),[2003]:rel(e4,x3,agent,0)]))])))"
  val fidoIsADogAndFidoWalks_drs = "alfa(top,drs([[1001]:x0],[[1001,1006]:named(x0,fido,per,0)]),drs([[1002]:p1,[1007]:e2],[[1002]:prop(p1,drs([[1003]:x3],[[1004]:pred(x3,dog,n,0),[1002]:eq(x0,x3)])),[1007]:pred(e2,walk,v,0),[1007]:rel(e2,x0,agent,0)]))"
  val fidoIsADogAndFidoDidNotWalk_drs = "alfa(top,drs([[1001]:x0],[[1001,1006]:named(x0,fido,per,0)]),drs([[1002]:p2],[[1002]:prop(p2,drs([],[[1004]:pred(x0,dog,n,0)])),[1008]:not(drs([[1009]:e3],[[1009]:pred(e3,walk,v,0),[1009]:rel(e3,x0,agent,0)]))]))"
  val aDogWalks_drs = "drs([[1001]:x0,[1003]:e1],[[1002]:pred(x0,dog,n,0),[1003]:pred(e1,walk,v,0),[1003]:rel(e1,x0,agent,0)])"
  val aDogDidNotWalk_drs = "drs([[1001]:x0],[[1002]:pred(x0,dog,n,0),[1004]:not(drs([[1005]:e1],[[1005]:pred(e1,walk,v,0),[1005]:rel(e1,x0,agent,0)]))])"
  val ifRoverIsADogThenRoverWalks_drs = "alfa(top,drs([[1002]:x0,[1007]:x1],[[1002]:named(x0,rover,per,0),[1007]:named(x1,rover,per,0)]),drs([],[[1001]:imp(drs([[1003]:p2],[[1003]:prop(p2,drs([[1004]:x3],[[1005]:pred(x3,dog,n,0),[1003]:eq(x0,x3)]))]),drs([[1008]:e4],[[1008]:pred(e4,walk,v,0),[1008]:rel(e4,x1,agent,0),[1006]:pred(e4,then,a,0)]))]))"

  val johnDidNotManageToLeave_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([],[[1003]:not(drs([[1004]:e1,[]:p2],[[1004]:pred(e1,manage,v,0),[1004]:rel(e1,x0,agent,0),[1004]:rel(e1,p2,theme,0),[1004]:prop(p2,drs([[1006]:e3],[[1006]:pred(e3,leave,v,0),[1006]:rel(e3,x0,agent,0)]))]))]))"
  val johnDidNotManageToNotForgetToLeave_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([],[[1003]:not(drs([[1004]:e1,[]:p2],[[1004]:pred(e1,manage,v,0),[1004]:rel(e1,x0,agent,0),[1004]:rel(e1,p2,theme,0),[1004]:prop(p2,drs([],[[1006]:not(drs([[1007]:e3,[]:p4],[[1007]:pred(e3,forget,v,0),[1007]:rel(e3,x0,agent,0),[1007]:rel(e3,p4,theme,0),[1007]:prop(p4,drs([[1009]:e5],[[1009]:pred(e5,leave,v,0),[1009]:rel(e5,x0,agent,0)]))]))]))]))]))"
  val johnDeclinedToLeave_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([[1002]:e1,[]:p2],[[1002]:pred(e1,decline,v,0),[1002]:rel(e1,x0,agent,0),[1002]:rel(e1,p2,theme,0),[1002]:prop(p2,drs([[1004]:e3],[[1004]:pred(e3,leave,v,0),[1004]:rel(e3,x0,agent,0)]))]))"
  val johnDidNotDeclineToLeave_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([],[[1003]:not(drs([[1004]:e1,[1006]:e2],[[1004]:pred(e1,decline,v,0),[1004]:rel(e1,x0,agent,0),[1006]:pred(e2,leave,v,0),[1006]:rel(e2,x0,agent,0)]))]))"
  val johnHesitatedToLeave_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([],[[1003]:not(drs([[1004]:e1,[]:p2],[[1004]:pred(e1,hesitate,v,0),[1004]:rel(e1,x0,agent,0),[1004]:rel(e1,p2,theme,0),[1004]:prop(p2,drs([[1006]:e3],[[1006]:pred(e3,leave,v,0),[1006]:rel(e3,x0,agent,0)]))]))]))"
  val johnDidNotHesitateToLeave_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([[1002]:e1,[]:p2],[[1002]:pred(e1,hesitate,v,0),[1002]:rel(e1,x0,agent,0),[1002]:rel(e1,p2,theme,0),[1002]:prop(p2,drs([[1004]:e3],[[1004]:pred(e3,leave,v,0),[1004]:rel(e3,x0,agent,0)]))]))"
  val edForgotToLockTheDoor_drs = "alfa(top,drs([[1001]:x0,[1005]:x1],[[1001]:named(x0,ed,per,0),[1006]:pred(x1,door,n,0)]),drs([[1002]:e2,[]:p3],[[1002]:pred(e2,forget,v,0),[1002]:rel(e2,x0,agent,0),[1002]:rel(e2,p3,theme,0),[1002]:prop(p3,drs([[1004]:e4],[[1004]:pred(e4,lock,v,0),[1004]:rel(e4,x0,agent,0),[1004]:rel(e4,x1,patient,0)]))]))"
  val edForgotThatHeLockedTheDoor_drs = "alfa(top,drs([[1001]:x0,[1006]:x1],[[1004]:pred(x0,male,a,0),[1001]:named(x0,ed,per,0),[1007]:pred(x1,door,n,0)]),drs([[1002]:e2,[]:p3],[[1002]:pred(e2,forget,v,0),[1002]:rel(e2,x0,agent,0),[1002]:rel(e2,p3,theme,0),[1002]:prop(p3,drs([[1005]:e4],[[1005]:pred(e4,lock,v,0),[1005]:rel(e4,x0,agent,0),[1005]:rel(e4,x1,patient,0)]))]))"
  val edForgotHeLockedTheDoor_drs = "alfa(top,drs([[1001]:x0,[1005]:x1],[[1003]:pred(x0,male,a,0),[1001]:named(x0,ed,per,0),[1006]:pred(x1,door,n,0)]),drs([[1002]:e2,[]:p3],[[1002]:pred(e2,forget,v,0),[1002]:rel(e2,x0,agent,0),[1002]:rel(e2,p3,theme,0),[1002]:prop(p3,drs([[1004]:e4],[[1004]:pred(e4,lock,v,0),[1004]:rel(e4,x0,agent,0),[1004]:rel(e4,x1,patient,0)]))]))"
  val edLockedTheDoor_drs = "alfa(top,drs([[1001]:x0,[1003]:x1],[[1001]:named(x0,ed,per,0),[1004]:pred(x1,door,n,0)]),drs([[1002]:e2],[[1002]:pred(e2,lock,v,0),[1002]:rel(e2,x0,agent,0),[1002]:rel(e2,x1,patient,0)]))"
  val edDidNotLockTheDoor_drs = "alfa(top,drs([[1001]:x0,[1005]:x1],[[1001]:named(x0,ed,per,0),[1006]:pred(x1,door,n,0)]),drs([],[[1003]:not(drs([[1004]:e2],[[1004]:pred(e2,lock,v,0),[1004]:rel(e2,x0,agent,0),[1004]:rel(e2,x1,patient,0)]))]))"

  val johnBoughtAConvertible_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([[1003]:x1,[1002]:e2],[[1004]:pred(x1,convertible,n,0),[1002]:pred(e2,buy,v,0),[1002]:rel(e2,x0,agent,0),[1002]:rel(e2,x1,patient,0)]))"
  val johnDidNotBuyAConvertible_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([],[[1003]:not(drs([[1005]:x1,[1004]:e2],[[1006]:pred(x1,convertible,n,0),[1004]:pred(e2,buy,v,0),[1004]:rel(e2,x0,agent,0),[1004]:rel(e2,x1,patient,0)]))]))"
  val johnBoughtACar_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([[1003]:x1,[1002]:e2],[[1004]:pred(x1,car,n,0),[1002]:pred(e2,buy,v,0),[1002]:rel(e2,x0,agent,0),[1002]:rel(e2,x1,patient,0)]))"
  val johnDidNotBuyACar_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([],[[1003]:not(drs([[1005]:x1,[1004]:e2],[[1006]:pred(x1,car,n,0),[1004]:pred(e2,buy,v,0),[1004]:rel(e2,x0,agent,0),[1004]:rel(e2,x1,patient,0)]))]))"
  val johnBoughtAnAutomobile_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([[1003]:x1,[1002]:e2],[[1004]:pred(x1,automobile,n,0),[1002]:pred(e2,buy,v,0),[1002]:rel(e2,x0,agent,0),[1002]:rel(e2,x1,patient,0)]))"
  val johnDidNotBuyAnAutomobile_drs = "alfa(top,drs([[1001]:x0],[[1001]:named(x0,john,per,0)]),drs([],[[1003]:not(drs([[1005]:x1,[1004]:e2],[[1006]:pred(x1,automobile,n,0),[1004]:pred(e2,buy,v,0),[1004]:rel(e2,x0,agent,0),[1004]:rel(e2,x1,patient,0)]))]))"

  //
  // Parses
  //
  val johnForgotToLeave_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "forgot", "forget", "VBD", "O")
    val w2 = Word(2, "to", "to", "TO", "O")
    val w3 = Word(3, "leave", "leave", "VB", "O")
    val w4 = Word(4, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("xcomp+to" -> Set(w3), "subj" -> Set(w0))
    w2.dependencies = Map()
    w3.dependencies = Map("subj" -> Set(w0))
    w4.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4))))
  }
  val johnDidNotForgetToLeave_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "did", "do", "VBD", "O")
    val w2 = Word(2, "not", "not", "RB", "O")
    val w3 = Word(3, "forget", "forget", "VB", "O")
    val w4 = Word(4, "to", "to", "TO", "O")
    val w5 = Word(5, "leave", "leave", "VB", "O")
    val w6 = Word(6, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("mod" -> Set(w2))
    w2.dependencies = Map()
    w3.dependencies = Map("xcomp+to" -> Set(w5), "subj" -> Set(w0), "aux" -> Set(w1))
    w4.dependencies = Map()
    w5.dependencies = Map("subj" -> Set(w0))
    w6.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4, w5, w6))))
  }
  val johnLeft_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "left", "leave", "VBD", "O")
    val w2 = Word(2, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("subj" -> Set(w0))
    w2.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2))))
  }
  val johnDidntLeave_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "did", "do", "VBD", "O")
    val w2 = Word(2, "not", "not", "RB", "O")
    val w3 = Word(3, "leave", "leave", "VB", "O")
    val w4 = Word(4, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("mod" -> Set(w2))
    w2.dependencies = Map()
    w3.dependencies = Map("subj" -> Set(w0), "aux" -> Set(w1))
    w4.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4))))
  }

  val someoneLeft_parse = {
    val w0 = Word(0, "Someone", "someone", "DT", "O")
    val w1 = Word(1, "left", "left", "NN", "O")
    val w2 = Word(2, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("det" -> Set(w0))
    w2.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2))))
  }
  val someoneDidntLeft_parse = {
    val w0 = Word(0, "Someone", "someone", "DT", "O")
    val w1 = Word(1, "did", "do", "VBD", "O")
    val w2 = Word(2, "not", "not", "RB", "O")
    val w3 = Word(3, "leave", "leave", "VB", "O")
    val w4 = Word(4, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("mod" -> Set(w2))
    w2.dependencies = Map()
    w3.dependencies = Map("subj" -> Set(w0), "aux" -> Set(w1))
    w4.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4))))
  }
  val everyDogWalks_parse = {
    val w0 = Word(0, "Every", "every", "DT", "O")
    val w1 = Word(1, "dog", "dog", "NN", "O")
    val w2 = Word(2, "walks", "walk", "VBZ", "O")
    val w3 = Word(3, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("det" -> Set(w0))
    w2.dependencies = Map("subj" -> Set(w1))
    w3.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3))))
  }
  val fidoIsADogAndFidoWalks_parse = {
    val w0 = Word(0, "Fido", "Fido", "NNP", "I-PER")
    val w1 = Word(1, "is", "be", "VBZ", "O")
    val w2 = Word(2, "a", "a", "DT", "O")
    val w3 = Word(3, "dog", "dog", "NN", "O")
    val w4 = Word(4, "and", "and", "CC", "O")
    val w5 = Word(5, "every", "every", "DT", "O")
    val w6 = Word(6, "dog", "dog", "NN", "O")
    val w7 = Word(7, "walks", "walk", "VBZ", "O")
    val w8 = Word(8, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("xcomp" -> Set(w3), "subj" -> Set(w0))
    w2.dependencies = Map()
    w3.dependencies = Map("det" -> Set(w2))
    w4.dependencies = Map("conj" -> Set(w7, w1))
    w5.dependencies = Map()
    w6.dependencies = Map("det" -> Set(w5))
    w7.dependencies = Map("subj" -> Set(w6))
    w8.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4, w5, w6, w7, w8))))
  }
  val fidoIsADogAndFidoDidNotWalk_parse = {
    val w0 = Word(0, "Fido", "Fido", "NNP", "I-PER")
    val w1 = Word(1, "is", "be", "VBZ", "O")
    val w2 = Word(2, "a", "a", "DT", "O")
    val w3 = Word(3, "dog", "dog", "NN", "O")
    val w4 = Word(4, "and", "and", "CC", "O")
    val w5 = Word(5, "fido", "fido", "NN", "O")
    val w6 = Word(6, "does", "do", "VBZ", "O")
    val w7 = Word(7, "not", "not", "RB", "O")
    val w8 = Word(8, "walk", "walk", "VB", "O")
    val w9 = Word(9, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("xcomp" -> Set(w3), "subj" -> Set(w0))
    w2.dependencies = Map()
    w3.dependencies = Map("det" -> Set(w2))
    w4.dependencies = Map("conj" -> Set(w6, w1))
    w5.dependencies = Map()
    w6.dependencies = Map("mod" -> Set(w7))
    w7.dependencies = Map()
    w8.dependencies = Map("subj" -> Set(w5), "aux" -> Set(w6))
    w9.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4, w5, w6, w7, w8, w9))))
  }
  val aDogWalks_parse = {
    val w0 = Word(0, "A", "a", "DT", "O")
    val w1 = Word(1, "dog", "dog", "NN", "O")
    val w2 = Word(2, "walks", "walk", "VBZ", "O")
    val w3 = Word(3, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("det" -> Set(w0))
    w2.dependencies = Map("subj" -> Set(w1))
    w3.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3))))
  }
  val aDogDidNotWalk_parse = {
    val w0 = Word(0, "A", "a", "DT", "O")
    val w1 = Word(1, "dog", "dog", "NN", "O")
    val w2 = Word(2, "did", "do", "VBD", "O")
    val w3 = Word(3, "not", "not", "RB", "O")
    val w4 = Word(4, "walk", "walk", "VB", "O")
    val w5 = Word(5, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("det" -> Set(w0))
    w2.dependencies = Map("mod" -> Set(w3))
    w3.dependencies = Map()
    w4.dependencies = Map("subj" -> Set(w1), "aux" -> Set(w2))
    w5.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4, w5))))
  }
  val ifRoverIsADogThenRoverWalks_parse = {
    val w0 = Word(0, "If", "if", "IN", "O")
    val w1 = Word(1, "Rover", "Rover", "NNP", "I-ORG")
    val w2 = Word(2, "is", "be", "VBZ", "O")
    val w3 = Word(3, "a", "a", "DT", "O")
    val w4 = Word(4, "dog", "dog", "NN", "O")
    val w5 = Word(5, "then", "then", "RB", "O")
    val w6 = Word(6, "Rover", "Rover", "NNP", "I-PER")
    val w7 = Word(7, "walks", "walk", "VBZ", "O")
    val w8 = Word(8, ".", ".", ".", "O")
    w0.dependencies = Map("ccomp" -> Set(w2))
    w1.dependencies = Map()
    w2.dependencies = Map("xcomp" -> Set(w4), "subj" -> Set(w1))
    w3.dependencies = Map()
    w4.dependencies = Map("det" -> Set(w3))
    w5.dependencies = Map()
    w6.dependencies = Map()
    w7.dependencies = Map("mod" -> Set(w5, w0), "subj" -> Set(w6))
    w8.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4, w5, w6, w7, w8))))
  }

  val johnDidNotManageToNotForgetToLeave_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "did", "do", "VBD", "O")
    val w2 = Word(2, "not", "not", "RB", "O")
    val w3 = Word(3, "manage", "manage", "VB", "O")
    val w4 = Word(4, "to", "to", "TO", "O")
    val w5 = Word(5, "not", "not", "RB", "O")
    val w6 = Word(6, "forget", "forget", "VB", "O")
    val w7 = Word(7, "to", "to", "TO", "O")
    val w8 = Word(8, "leave", "leave", "VB", "O")
    val w9 = Word(9, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("mod" -> Set(w2))
    w2.dependencies = Map()
    w3.dependencies = Map("xcomp+to" -> Set(w6), "subj" -> Set(w0), "aux" -> Set(w1))
    w4.dependencies = Map("mod" -> Set(w5))
    w5.dependencies = Map()
    w6.dependencies = Map("xcomp+to" -> Set(w8), "subj" -> Set(w0))
    w7.dependencies = Map()
    w8.dependencies = Map("subj" -> Set(w0))
    w9.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4, w5, w6, w7, w8, w9))))
  }
  val johnDeclinedToLeave_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "declined", "decline", "VBD", "O")
    val w2 = Word(2, "to", "to", "TO", "O")
    val w3 = Word(3, "leave", "leave", "VB", "O")
    val w4 = Word(4, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("xcomp+to" -> Set(w3), "subj" -> Set(w0))
    w2.dependencies = Map()
    w3.dependencies = Map("subj" -> Set(w0))
    w4.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4))))
  }
  val johnDidNotDeclineToLeave_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "did", "do", "VBD", "O")
    val w2 = Word(2, "not", "not", "RB", "O")
    val w3 = Word(3, "decline", "decline", "VB", "O")
    val w4 = Word(4, "to", "to", "TO", "O")
    val w5 = Word(5, "leave", "leave", "VB", "O")
    val w6 = Word(6, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("mod" -> Set(w2))
    w2.dependencies = Map()
    w3.dependencies = Map("mod" -> Set(w4), "subj" -> Set(w0), "aux" -> Set(w1))
    w4.dependencies = Map()
    w5.dependencies = Map()
    w6.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4, w5, w6))))
  }
  val johnHesitatedToLeave_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "did", "do", "VBD", "O")
    val w2 = Word(2, "not", "not", "RB", "O")
    val w3 = Word(3, "hesitate", "hesitate", "VB", "O")
    val w4 = Word(4, "to", "to", "TO", "O")
    val w5 = Word(5, "leave", "leave", "VB", "O")
    val w6 = Word(6, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("mod" -> Set(w2))
    w2.dependencies = Map()
    w3.dependencies = Map("xcomp+to" -> Set(w5), "subj" -> Set(w0), "aux" -> Set(w1))
    w4.dependencies = Map()
    w5.dependencies = Map("subj" -> Set(w0))
    w6.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4, w5, w6))))
  }
  val johnDidNotHesitateToLeave_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "hesitated", "hesitate", "VBD", "O")
    val w2 = Word(2, "to", "to", "TO", "O")
    val w3 = Word(3, "leave", "leave", "VB", "O")
    val w4 = Word(4, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("xcomp+to" -> Set(w3), "subj" -> Set(w0))
    w2.dependencies = Map()
    w3.dependencies = Map("subj" -> Set(w0))
    w4.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4))))
  }
  val edForgotToLockTheDoor_parse = {
    val s1_w0 = Word(0, "Ed", "Ed", "NNP", "I-PER")
    val s1_w1 = Word(1, "forgot", "forget", "VBD", "O")
    val s1_w2 = Word(2, "to", "to", "TO", "O")
    val s1_w3 = Word(3, "lock", "lock", "VB", "O")
    val s1_w4 = Word(4, "the", "the", "DT", "O")
    val s1_w5 = Word(5, "door", "door", "NN", "O")
    val s1_w6 = Word(6, ".", ".", ".", "O")
    s1_w0.dependencies = Map()
    s1_w1.dependencies = Map("xcomp+to" -> Set(s1_w3), "subj" -> Set(s1_w0))
    s1_w2.dependencies = Map()
    s1_w3.dependencies = Map("subj" -> Set(s1_w0), "obj" -> Set(s1_w5))
    s1_w4.dependencies = Map()
    s1_w5.dependencies = Map("det" -> Set(s1_w4))
    s1_w6.dependencies = Map()
    Discourse("0", List(Sentence(1, List(s1_w0, s1_w1, s1_w2, s1_w3, s1_w4, s1_w5, s1_w6))))
  }
  val edForgotThatHeLockedTheDoor_parse = {
    val s1_w0 = Word(0, "Ed", "Ed", "NNP", "I-PER")
    val s1_w1 = Word(1, "forgot", "forget", "VBD", "O")
    val s1_w2 = Word(2, "that", "that", "IN", "O")
    val s1_w3 = Word(3, "he", "he", "PRP", "O")
    val s1_w4 = Word(4, "locked", "lock", "VBD", "O")
    val s1_w5 = Word(5, "the", "the", "DT", "O")
    val s1_w6 = Word(6, "door", "door", "NN", "O")
    val s1_w7 = Word(7, ".", ".", ".", "O")
    s1_w0.dependencies = Map()
    s1_w1.dependencies = Map("subj" -> Set(s1_w0), "ccomp+that" -> Set(s1_w4))
    s1_w2.dependencies = Map()
    s1_w3.dependencies = Map()
    s1_w4.dependencies = Map("subj" -> Set(s1_w3), "obj" -> Set(s1_w6))
    s1_w5.dependencies = Map()
    s1_w6.dependencies = Map("det" -> Set(s1_w5))
    s1_w7.dependencies = Map()
    Discourse("0", List(Sentence(1, List(s1_w0, s1_w1, s1_w2, s1_w3, s1_w4, s1_w5, s1_w6, s1_w7))))
  }
  val edForgotHeLockedTheDoor_parse = {
    val s1_w0 = Word(0, "Ed", "Ed", "NNP", "I-PER")
    val s1_w1 = Word(1, "forgot", "forget", "VBD", "O")
    val s1_w2 = Word(2, "he", "he", "PRP", "O")
    val s1_w3 = Word(3, "locked", "lock", "VBD", "O")
    val s1_w4 = Word(4, "the", "the", "DT", "O")
    val s1_w5 = Word(5, "door", "door", "NN", "O")
    val s1_w6 = Word(6, ".", ".", ".", "O")
    s1_w0.dependencies = Map()
    s1_w1.dependencies = Map("subj" -> Set(s1_w0), "ccomp" -> Set(s1_w3))
    s1_w2.dependencies = Map()
    s1_w3.dependencies = Map("subj" -> Set(s1_w2), "obj" -> Set(s1_w5))
    s1_w4.dependencies = Map()
    s1_w5.dependencies = Map("det" -> Set(s1_w4))
    s1_w6.dependencies = Map()
    Discourse("0", List(Sentence(1, List(s1_w0, s1_w1, s1_w2, s1_w3, s1_w4, s1_w5, s1_w6))))
  }
  val edLockedTheDoor_parse = {
    val s1_w0 = Word(0, "Ed", "Ed", "NNP", "I-PER")
    val s1_w1 = Word(1, "locked", "lock", "VBD", "O")
    val s1_w2 = Word(2, "the", "the", "DT", "O")
    val s1_w3 = Word(3, "door", "door", "NN", "O")
    val s1_w4 = Word(4, ".", ".", ".", "O")
    s1_w0.dependencies = Map()
    s1_w1.dependencies = Map("subj" -> Set(s1_w0), "obj" -> Set(s1_w3))
    s1_w2.dependencies = Map()
    s1_w3.dependencies = Map("det" -> Set(s1_w2))
    s1_w4.dependencies = Map()
    Discourse("0", List(Sentence(1, List(s1_w0, s1_w1, s1_w2, s1_w3, s1_w4))))
  }
  val edDidNotLockTheDoor_parse = {
    val s1_w0 = Word(0, "Ed", "Ed", "NNP", "I-PER")
    val s1_w1 = Word(1, "did", "do", "VBD", "O")
    val s1_w2 = Word(2, "not", "not", "RB", "O")
    val s1_w3 = Word(3, "lock", "lock", "VB", "O")
    val s1_w4 = Word(4, "the", "the", "DT", "O")
    val s1_w5 = Word(5, "door", "door", "NN", "O")
    val s1_w6 = Word(6, ".", ".", ".", "O")
    s1_w0.dependencies = Map()
    s1_w1.dependencies = Map("mod" -> Set(s1_w2))
    s1_w2.dependencies = Map()
    s1_w3.dependencies = Map("subj" -> Set(s1_w0), "obj" -> Set(s1_w5), "aux" -> Set(s1_w1))
    s1_w4.dependencies = Map()
    s1_w5.dependencies = Map("det" -> Set(s1_w4))
    s1_w6.dependencies = Map()
    Discourse("0", List(Sentence(1, List(s1_w0, s1_w1, s1_w2, s1_w3, s1_w4, s1_w5, s1_w6))))
  }

  val johnBoughtAConvertible_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "bought", "buy", "VBD", "O")
    val w2 = Word(2, "a", "a", "DT", "O")
    val w3 = Word(3, "convertible", "convertible", "JJ", "O")
    val w4 = Word(4, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("subj" -> Set(w0), "obj" -> Set(w3))
    w2.dependencies = Map()
    w3.dependencies = Map("det" -> Set(w2))
    w4.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4))))
  }
  val johnDidNotBuyAConvertible_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "did", "do", "VBD", "O")
    val w2 = Word(2, "not", "not", "RB", "O")
    val w3 = Word(3, "buy", "buy", "VB", "O")
    val w4 = Word(4, "a", "a", "DT", "O")
    val w5 = Word(5, "convertible", "convertible", "JJ", "O")
    val w6 = Word(6, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("mod" -> Set(w2))
    w2.dependencies = Map()
    w3.dependencies = Map("subj" -> Set(w0), "obj" -> Set(w5), "aux" -> Set(w1))
    w4.dependencies = Map()
    w5.dependencies = Map("det" -> Set(w4))
    w6.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4, w5, w6))))
  }
  val johnBoughtACar_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "bought", "buy", "VBD", "O")
    val w2 = Word(2, "a", "a", "DT", "O")
    val w3 = Word(3, "car", "car", "NN", "O")
    val w4 = Word(4, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("subj" -> Set(w0), "obj" -> Set(w3))
    w2.dependencies = Map()
    w3.dependencies = Map("det" -> Set(w2))
    w4.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4))))
  }
  val johnDidNotBuyACar_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "did", "do", "VBD", "O")
    val w2 = Word(2, "not", "not", "RB", "O")
    val w3 = Word(3, "buy", "buy", "VB", "O")
    val w4 = Word(4, "a", "a", "DT", "O")
    val w5 = Word(5, "car", "car", "NN", "O")
    val w6 = Word(6, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("mod" -> Set(w2))
    w2.dependencies = Map()
    w3.dependencies = Map("subj" -> Set(w0), "obj" -> Set(w5), "aux" -> Set(w1))
    w4.dependencies = Map()
    w5.dependencies = Map("det" -> Set(w4))
    w6.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4, w5, w6))))
  }
  val johnBoughtAnAutomobile_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "bought", "buy", "VBD", "O")
    val w2 = Word(2, "an", "an", "DT", "O")
    val w3 = Word(3, "automobile", "automobile", "NN", "O")
    val w4 = Word(4, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("subj" -> Set(w0), "obj" -> Set(w3))
    w2.dependencies = Map()
    w3.dependencies = Map("det" -> Set(w2))
    w4.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4))))
  }
  val johnDidNotBuyAnAutomobile_parse = {
    val w0 = Word(0, "John", "John", "NNP", "I-PER")
    val w1 = Word(1, "did", "do", "VBD", "O")
    val w2 = Word(2, "not", "not", "RB", "O")
    val w3 = Word(3, "buy", "buy", "VB", "O")
    val w4 = Word(4, "an", "an", "DT", "O")
    val w5 = Word(5, "automobile", "automobile", "NN", "O")
    val w6 = Word(6, ".", ".", ".", "O")
    w0.dependencies = Map()
    w1.dependencies = Map("mod" -> Set(w2))
    w2.dependencies = Map()
    w3.dependencies = Map("subj" -> Set(w0), "obj" -> Set(w5), "aux" -> Set(w1))
    w4.dependencies = Map()
    w5.dependencies = Map("det" -> Set(w4))
    w6.dependencies = Map()
    Discourse("0", List(Sentence(1, List(w0, w1, w2, w3, w4, w5, w6))))
  }
  val johnDidNotManageToLeave_parse = {
    val s1_w0 = Word(0, "John", "John", "NNP", "I-PER")
    val s1_w1 = Word(1, "did", "do", "VBD", "O")
    val s1_w2 = Word(2, "not", "not", "RB", "O")
    val s1_w3 = Word(3, "manage", "manage", "VB", "O")
    val s1_w4 = Word(4, "to", "to", "TO", "O")
    val s1_w5 = Word(5, "leave", "leave", "VB", "O")
    val s1_w6 = Word(6, ".", ".", ".", "O")
    s1_w0.dependencies = Map()
    s1_w1.dependencies = Map("mod" -> Set(s1_w2))
    s1_w2.dependencies = Map()
    s1_w3.dependencies = Map("xcomp+to" -> Set(s1_w5), "subj" -> Set(s1_w0), "aux" -> Set(s1_w1))
    s1_w4.dependencies = Map()
    s1_w5.dependencies = Map("subj" -> Set(s1_w0))
    s1_w6.dependencies = Map()
    Discourse("0", List(Sentence(1, List(s1_w0, s1_w1, s1_w2, s1_w3, s1_w4, s1_w5, s1_w6))))
  }

  class FakeBoxerDiscourseInterpreter(result: String) extends DiscourseInterpreter[BoxerExpression] {
    override def batchInterpretMultisentence(inputs: List[List[String]], discourseIds: Option[List[String]] = None, question: Boolean = false, verbose: Boolean = false): List[Option[BoxerExpression]] = {
      return List(Some(List(
        new MergingBoxerExpressionInterpreterDecorator()).map(_.interpret _).reduceLeft(_ andThen _)(
          new BoxerExpressionParser().parse(result))))
    }
  }

  class FakeCandc(result: Discourse) extends DiscourseParser[Discourse] {
    override def batchParseMultisentence(inputs: List[List[String]], args: Map[String, String] = Map(), discourseIds: Option[Seq[String]] = None, model: Option[String] = None, verbose: Boolean = false): List[Option[Discourse]] = {
      return List(Some(result))
    }
  }
}

class ModalTestsData {
  def test() {
    val sList = List(edLockedTheDoor, edDidNotLockTheDoor)

    for (s <- sList) {
      if (true) { // Generate DRS
        val boxerDiscourseInterpreter = new BoxerDiscourseInterpreter[BoxerExpression](
          new PassthroughBoxerExpressionInterpreter(),
          new CandcImpl(),
          new BoxerImpl())
        val b = boxerDiscourseInterpreter.interpretMultisentence(List(s))
        println(b)
        println(new Boxer2DrtExpressionInterpreter().interpret(b).pretty)
      }

      if (true) { // Generate Parse
        val candc = new CandcDiscourseParser(new CandcImpl())
        println(candc.parseMultisentence(List(s)).repr)
      }
    }

  }

}
