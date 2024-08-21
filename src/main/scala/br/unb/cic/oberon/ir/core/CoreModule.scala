package br.unb.cic.oberon.ir.ast

import br.unb.cic.oberon.visitor.OberonVisitor
import br.unb.cic.oberon.environment.Environment
import br.unb.cic.oberon.interpreter.Interpreter

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

/** Abstract representation of an Oberon Module
 *
 * Here we use an object-oriented decomposition to represent
 * Oberon concepts as Scala classes and traits.
 *
 * The use of case classes here are handy, mostly because we
 * can use case case classes in pattern matching, and they also
 * provide useful methods (such as equality).
 *
 * Traits offers an interesting approach for software composition,
 * besides OO inheritance.
 */
case class CoreModule(name: String,
                        constants: List[Constant],
                        variables: List[VariableDeclaration],
                        procedures: List[Procedure],
                        exp: Option[expression]
                       ) {
  def accept(v: OberonVisitor): v.T = v.visit(this)
}

// SequenceStatement(List[Stmt]) extends Stmt
// alternativa: SequenceStatement(stmt, stmt) extends Stmt

/* procedure declaration definition */
case class Procedure(
    name: String,
    args: List[FormalArg],
    returnType: Option[Type],
    constants: List[Constant],
    variables: List[VariableDeclaration],
    exp: expression
) {
  def accept(v: OberonVisitor): v.T = v.visit(this)
}

/* formal argument definition */
sealed trait FormalArg {
  def accept(v: OberonVisitor): v.T = v.visit(this)
  def argumentType: Type
  def name: String
}
case class ParameterByValue(name: String, argumentType: Type) extends FormalArg
case class ParameterByReference(name: String, argumentType: Type)
    extends FormalArg

/* Imports */
case class Import(name: String) {
  def accept(v: OberonVisitor): v.T = v.visit(this)
}

/* Constant definition */
case class Constant(name: String, exp: expression) {
  def accept(v: OberonVisitor): v.T = v.visit(this)
}

/* Variable declaration definition */
case class VariableDeclaration(name: String, variableType: Type) {
  def accept(v: OberonVisitor): v.T = v.visit(this)
}

/* expressions */
trait expression {
  def accept(v: OberonVisitor): v.T = v.visit(this)
}

sealed abstract class Value extends expression with Ordered[Value] {
  type T
  def value: T

  override def compare(that: Value): Int = (this, that) match {
    case (v1: IntValue, v2: IntValue) => v1.value.compareTo(v2.value)
    case (v1: IntValue, v2: RealValue) =>
      ValueConversion.intValue2RealValue(v1).compareTo(v2)
    case (v1: RealValue, v2: IntValue) =>
      ValueConversion.intValue2RealValue(v2).compareTo(v1)
    case (v1: RealValue, v2: RealValue)     => v1.value.compareTo(v2.value)
    case (v1: CharValue, v2: CharValue)     => v1.value.compareTo(v2.value)
    case (v1: StringValue, v2: StringValue) => v1.value.compareTo(v2.value)
    case _ =>
      throw new RuntimeException(
        "Comparison is not defined for " + this.getClass + " and " + that.getClass
      )
  }
}

sealed trait Number extends expression {
  def +(that: Number): Number
  def -(that: Number): Number
  def *(that: Number): Number
  def /(that: Number): Number

}

sealed trait Modular extends Number {
  def mod(that: Modular): Modular
}

case class IntValue(value: Int) extends Value with Modular {
  type T = Int
  def +(that: Number): Number = that match {
    case other: IntValue  => IntValue(value + other.value)
    case other: RealValue => RealValue(value + other.value)
  }

  def -(that: Number): Number = that match {
    case other: IntValue  => IntValue(value - other.value)
    case other: RealValue => RealValue(value - other.value)
  }

  def *(that: Number): Number = that match {
    case other: IntValue  => IntValue(value * other.value)
    case other: RealValue => RealValue(value * other.value)
  }

  def /(that: Number): Number = that match {
    case other: IntValue  => IntValue(value / other.value)
    case other: RealValue => RealValue(value / other.value)
  }

  val positiveMod = (x: Int, y: Int) => {
    val res = x % y; if (x < 0) res + y else res
  }

  def mod(that: Modular): Modular = that match {
    case other: IntValue => IntValue(positiveMod(value, other.value))
  }
}

case class RealValue(value: Double) extends Value with Number {
  type T = Double

  def +(that: Number): Number = that match {
    case other: IntValue  => RealValue(value + other.value)
    case other: RealValue => RealValue(value + other.value)
  }

  def -(that: Number): Number = that match {
    case other: IntValue  => RealValue(value - other.value)
    case other: RealValue => RealValue(value - other.value)
  }

  def *(that: Number): Number = that match {
    case other: IntValue  => RealValue(value * other.value)
    case other: RealValue => RealValue(value * other.value)
  }

  def /(that: Number): Number = that match {
    case other: IntValue  => RealValue(value / other.value)
    case other: RealValue => RealValue(value / other.value)
  }
}

case class CharValue(value: Char) extends Value { type T = Char }
case class StringValue(value: String) extends Value { type T = String }
case class BoolValue(value: Boolean) extends Value { type T = Boolean }
case object NullValue extends Value {
  type T = Unit
  def value: T = ()
}

case class Location(loc: Int) extends expression
case class Brackets(exp: expression) extends expression
case class ArrayValue(value: ListBuffer[expression], arrayType: ArrayType)
    extends Value { type T = ListBuffer[expression] }
case class ArraySubscript(arrayBase: expression, index: expression)
    extends expression
case class Undef() extends expression
case class FieldAccessexpression(exp: expression, name: String)
    extends expression
case class PointerAccessexpression(name: String) extends expression
case class Varexpression(name: String) extends expression
case class FunctionCallexpression(name: String, args: List[expression])
    extends expression
case class EQexpression(left: expression, right: expression) extends expression
case class NEQexpression(left: expression, right: expression) extends expression
case class GTexpression(left: expression, right: expression) extends expression
case class LTexpression(left: expression, right: expression) extends expression
case class GTEexpression(left: expression, right: expression) extends expression
case class LTEexpression(left: expression, right: expression) extends expression
case class Addexpression(left: expression, right: expression) extends expression
case class Subexpression(left: expression, right: expression) extends expression
case class Multexpression(left: expression, right: expression)
    extends expression
case class Divexpression(left: expression, right: expression) extends expression
case class Orexpression(left: expression, right: expression) extends expression
case class Andexpression(left: expression, right: expression) extends expression
case class Modexpression(left: expression, right: expression) extends expression
case class Notexpression(exp: expression) extends expression
case class Lambdaexpression(args: List[FormalArg], exp: expression)
    extends expression

/* Statements */
trait Statement {
  val label = Statement.getLabel()
  def accept(v: OberonVisitor): v.T = v.visit(this)
}

object Statement {
  var label = 0

  def getLabel(): Int = {
    label += 1
    label
  }

  def reset(): Unit = {
    label = 0
  }
}

case class Assignmentexp(designator: Designator, exp: expression)
    extends expression
case class Sequenceexp(exps: List[expression]) extends expression
case class ReadLongRealexp(varName: String) extends expression
case class ReadRealexp(varName: String) extends expression
case class ReadLongIntexp(varName: String) extends expression
case class ReadIntexp(varName: String) extends expression
case class ReadShortIntexp(varName: String) extends expression
case class ReadCharexp(varName: String) extends expression
case class WriteSexp(expression: expression) extends expression
case class ProcedureCallexp(name: String, args: List[expression])
    extends expression
case class TestCallexp(name: String) extends expression
case class IfElseexp(
    condition: expression,
    thenexp: expression,
    elseexp: Option[expression]
) extends expression
case class IfElseIfexp(
    condition: expression,
    thenexp: expression,
    elseifexp: List[ElseIfexp],
    elseexp: Option[expression]
) extends expression
case class ElseIfexp(condition: expression, thenexp: expression)
    extends expression
case class WhileExp(condition: expression, exp: expression) extends expression
case class RepeatUntilExp(condition: expression, exp: expression)
    extends expression
case class ForExp(init: expression, condition: expression, exp: expression)
    extends expression
case class ForEachExp(varName: String, exp: expression, exp: expression)
    extends expression
case class LoopExp(exp: expression) extends expression
case class ReturnExp(exp: expression) extends expression
case class CaseExp(
    exp: expression,
    cases: List[CaseAlternative],
    elseExp: Option[expression]
) extends expression
case class ExitExp() extends expression
case class NewExp(varName: String) extends expression
case class MetaExp(f: () => expression) extends expression
case class AssertTrueExp(exp: expression) extends expression
case class AssertEqualExp(left: expression, right: expression) extends expression
case class AssertNotEqualExp(left: expression, right: expression) extends expression
case class AssertError() extends expression

trait CaseAlternative {
  def accept(v: OberonVisitor): v.T = v.visit(this)
}

case class SimpleCase(condition: expression, exp: expression)
    extends CaseAlternative
case class RangeCase(min: expression, max: expression, exp: expression)
    extends CaseAlternative

sealed trait Designator

case class VarAssignment(varName: String) extends Designator
case class ArrayAssignment(array: expression, index: expression)
    extends Designator
case class RecordAssignment(record: expression, field: String)
    extends Designator
case class PointerAssignment(pointerName: String) extends Designator

/** User defined types.
  *
  * Users can declare either records or array types.
  */
case class UserDefinedType(name: String, baseType: Type) {
  def accept(v: OberonVisitor): v.T = v.visit(this)
}

/** The hierarchy for the Oberon supported types */
sealed trait Type {
  def accept(v: OberonVisitor): v.T = v.visit(this)
}

case object IntegerType extends Type
case object RealType extends Type
case object BooleanType extends Type
case object CharacterType extends Type
case object StringType extends Type
case object UndefinedType extends Type
case object NullType extends Type
case object LocationType extends Type

case class RecordType(variables: List[VariableDeclaration]) extends Type
case class ArrayType(length: Int, baseType: Type) extends Type
case class PointerType(variableType: Type) extends Type
case class LambdaType(argsTypes: List[Type], returnType: Type) extends Type

case class ReferenceToUserDefinedType(name: String) extends Type

/* useful for implementing the REPL feature */
trait REPL

case class REPLexpression(exp: expression) extends REPL
case class REPLStatement(exp: Statement) extends REPL
case class REPLVarDeclaration(declarations: List[VariableDeclaration])
    extends REPL
case class REPLConstant(constants: Constant) extends REPL
case class REPLUserTypeDeclaration(userTypes: UserDefinedType) extends REPL

object ValueConversion {
  def intValue2RealValue(intValue: IntValue): RealValue = RealValue(
    intValue.value.toDouble
  )
  def charValue2IntValue(charValue: CharValue): IntValue = IntValue(
    charValue.value.toInt
  )
}
