package br.unb.cic.oberon.transformations

import br.unb.cic.oberon.ir.ast._
import br.unb.cic.oberon.ir.common._
import br.unb.cic.oberon.ir.core._
import br.unb.cic.oberon.visitor.OberonVisitorAdapter

import scala.collection.mutable.ListBuffer
//import br.unb.cic.oberon.ir.ast.Procedure

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable._
import _root_.br.unb.cic.oberon.ir.core.WhileExp



object CoreMTransformer {

  def flatSequenceOfStatements(stmts: List[Statement]): List[Statement] =
      stmts.flatMap {
        case SequenceStmt(ss) => flatSequenceOfStatements(ss)
        case s => List(s)
      }
    
  def reduceToCoreExpression(
                             stmt: Statement,
                             caseIdGenerator: AtomicInteger = new AtomicInteger(0),
                             addedVariables: ArrayBuffer[VariableDeclaration] =
                             new ArrayBuffer[VariableDeclaration]
                           ): Expression = {
    stmt match {
      case SequenceStmt(stmts) => 
            SequenceExp(Tuple2(reduceToCoreExpression(stmts(0)),reduceToCoreExpression(SequenceStmt(stmts.drop(0))))
            )
      case LoopStmt(stmt) =>
        WhileExp(
          BoolValue(true),
          reduceToCoreExpression(stmt, caseIdGenerator, addedVariables)
        )
      case RepeatUntilStmt(condition, stmt) =>
        WhileExp(
          BoolValue(true),
            SequenceExp(
              Tuple2(
                reduceToCoreExpression(stmt, caseIdGenerator, addedVariables),
                reduceToCoreExpression(
                  IfElseStmt(condition, ExitStmt(), None),
                  caseIdGenerator,
                  addedVariables
                )
              )
          )
        )

      case ForStmt(initStmt, condition, block) =>
        SequenceExp(
          Tuple2(
            reduceToCoreExpression(initStmt, caseIdGenerator, addedVariables),
            WhileExp(
              condition,
              reduceToCoreExpression(block, caseIdGenerator, addedVariables)
            )
          )
        )

      case IfElseIfStmt(condition, thenStmt, elsifStmt, elseStmt) =>
        IfElseExp(
          condition,
          reduceToCoreExpression(thenStmt, caseIdGenerator, addedVariables),
          Some(
            reduceElsifStatement(
              elsifStmt,
              elseStmt,
              caseIdGenerator,
              addedVariables
            )
          )
        )

      case CaseStmt(exp, cases, elseStmt) =>
        reduceCaseStatement(
          exp,
          cases,
          elseStmt,
          caseIdGenerator,
          addedVariables
        )
      case WhileStmt(condition, stmt) =>
        WhileExp(
          condition,
          reduceToCoreExpression(stmt, caseIdGenerator, addedVariables)
        )

      case AssertEqualStmt(left, right) =>
        AssertTrueExp(EQExpression(left, right))

      case AssertNotEqualStmt(left, right) =>
        AssertTrueExp(NEQExpression(left, right))

      case AssertError() =>
        AssertTrueExp(BoolValue(false))

      case _ => ???
    }
    
      
  }

  private def reduceElsifStatement(
                                      elsifStmts: List[ElseIfStmt],
                                      elseStmt: Option[Statement],
                                      caseIdGenerator: AtomicInteger,
                                      addedVariables: ArrayBuffer[VariableDeclaration]
                                    ): Expression =
      elsifStmts match {
        case currentElsif :: Nil =>
          IfElseExp(
            currentElsif.condition,
            reduceToCoreExpression(
              currentElsif.thenStmt,
              caseIdGenerator,
              addedVariables
            ),
            elseStmt.map((stmt) =>
              reduceToCoreExpression(stmt, caseIdGenerator, addedVariables)
            )
          )
        case currentElsif :: tail =>
          IfElseExp(
            currentElsif.condition,
            reduceToCoreExpression(
              currentElsif.thenStmt,
              caseIdGenerator,
              addedVariables
            ),
            Some(
              reduceElsifStatement(
                tail,
                elseStmt.map((stmt) =>
                  CoreTransformer.reduceToCoreStatement(stmt, caseIdGenerator, addedVariables)
                ),
                caseIdGenerator,
                addedVariables
              )
            )
          )
        case Nil =>
          throw new IllegalArgumentException("elsifExp cannot be empty.")
      }
     private def reduceCaseStatement(
                                     exp: Expression,
                                     cases: List[CaseAlternative],
                                     elseStmt: Option[Statement],
                                     caseIdGenerator: AtomicInteger,
                                     addedVariables: ArrayBuffer[VariableDeclaration]
                                   ): Expression = {
      val coreElseStmt = elseStmt.map((stmt) =>
        reduceToCoreExpression(stmt, caseIdGenerator, addedVariables)
      )

      val caseExpressionId = exp match {
        case VarExpression(name) => name
        case _ => {
          f"case_exp#${caseIdGenerator.getAndIncrement()}"
        }
      }
      val caseExpressionEvaluation =
        AssignmentExp(VarAssignment(caseExpressionId), exp)

      def casesToIfElseExp(cases: List[CaseAlternative]): IfElseExp =
        cases match {
          case SimpleCase(condition, stmt) :: Nil =>
            val newCondition =
              EQExpression(VarExpression(caseExpressionId), condition)
            IfElseExp(
              newCondition,
              reduceToCoreExpression(
                stmt,
                caseIdGenerator,
                (VariableDeclaration(
                  caseExpressionId,
                  IntegerType
                ) :: addedVariables.toList).to(ArrayBuffer)
              ),
              coreElseStmt
            )

          case SimpleCase(condition, stmt) :: tailCases =>
            val newCondition =
              EQExpression(VarExpression(caseExpressionId), condition)
            val newElse = Some(casesToIfElseExp(tailCases))
            IfElseExp(
              newCondition,
              reduceToCoreExpression(
                stmt,
                caseIdGenerator,
                (VariableDeclaration(
                  caseExpressionId,
                  IntegerType
                ) :: addedVariables.toList).to(ArrayBuffer)
              ),
              newElse
            )

          case RangeCase(min, max, stmt) :: Nil =>
            val newCondition = AndExpression(
              LTEExpression(min, VarExpression(caseExpressionId)),
              LTEExpression(VarExpression(caseExpressionId), max)
            )
            IfElseExp(
              newCondition,
              reduceToCoreExpression(
                stmt,
                caseIdGenerator,
                (VariableDeclaration(
                  caseExpressionId,
                  IntegerType
                ) :: addedVariables.toList).to(ArrayBuffer)
              ),
              coreElseStmt
            )

          case RangeCase(min, max, stmt) :: tailCases =>
            val newCondition = AndExpression(
              LTEExpression(min, VarExpression(caseExpressionId)),
              LTEExpression(VarExpression(caseExpressionId), max)
            )
            val newElse = Some(casesToIfElseExp(tailCases))
            IfElseExp(
              newCondition,
              reduceToCoreExpression(
                stmt,
                caseIdGenerator,
                (VariableDeclaration(
                  caseExpressionId,
                  IntegerType
                ) :: addedVariables.toList).to(ArrayBuffer)
              ),
              newElse
            )

          case _ => throw new RuntimeException("Invalid CaseStmt without cases")
        }

      exp match {
        case VarExpression(_) => casesToIfElseExp(cases)
        case _ =>
          // VariableDeclaration(caseExpressionId, IntegerType) +: addedVariables
          addedVariables.appendAll(
            List(VariableDeclaration(caseExpressionId, IntegerType))
          )
          SequenceExp(Tuple2(caseExpressionEvaluation, casesToIfElseExp(cases)))
      }
    }
}

