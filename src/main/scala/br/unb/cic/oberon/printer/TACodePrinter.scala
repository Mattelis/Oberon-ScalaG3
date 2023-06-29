package br.unb.cic.oberon.printer

import br.unb.cic.oberon.ir.tac.{AddOp, Address, AndOp, Constant, CopyOp, DivOp, MulOp, Name, NotOp, OrOp, SubOp, TAC, Temporary}
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.{line, text}

/**
 * This class has the responsibility to print TAC Abstraction in Scala
 * We wanted an string visualization of the abstraction
 * We can also get a Doc representation using an external library (paiges)
 * @author Marcelo M. Amorim
 * @since 28/06/2023
 */
object TACodePrinter {

  /**
   *
   * @param instructions:
   * @return
   */
  private def buildDocument(instructions: List[TAC]): Doc = {
    val tacHeader = Doc.text("")
    instructions.foldLeft(tacHeader)(generateCode)
  }

  private def generateCode(tac: Doc, instruction: TAC): Doc = {

    instruction match {
      case AddOp(s1, s2, dest, label) => tac / text(handleTAC(dest, s1, s2, "+"))
      case SubOp(s1, s2, dest, label) => tac / text(handleTAC(dest, s1, s2, "-"))
      case MulOp(s1, s2, dest, label) => tac / text(handleTAC(dest, s1, s2, "*"))
      case DivOp(s1, s2, dest, label) => tac / text(handleTAC(dest, s1, s2, "/"))
      case AndOp(s1, s2, dest, label) => tac / text(handleTAC(dest, s1, s2, "&&"))
      case OrOp(s1, s2, dest, label) => tac / text(handleTAC(dest, s1, s2, "||"))
      case NotOp(s1, dest, label) => tac / text(s"${handleAddress(dest)} = not ${handleAddress(s1)}")
      case CopyOp(s1, dest, label) => tac / text(s"${handleAddress(dest)} = ${handleAddress(s1)}")
      case _ => Doc.text("Not implemented in printer")
    }

  }

  /**
   *
   * @param destiny of operation
   * @param s1 storage 1
   * @param s2 storage 2
   * @param operation of instruction
   * @return
   */
  private def handleTAC(destiny : Address, s1 : Address, s2: Address, operation : String): String = {
    s"${handleAddress(destiny)} = ${handleAddress(s1)} $operation ${handleAddress(s2)}"
  }

  /**
   * @param address address to be handled
   */
  private def handleAddress(address : Address): String = {
    address match {
      case Temporary(t, number, p) => s"t$number"
      case Constant(value, t) => s"$value"
      case Name(id, t) => s"$id"
    }
  }

  /**
   * This method print all itens in instructions list
   *
   * @param instructions : reference to instructions list
   */
  def printInstructionSequence(instructions: List[TAC]): Unit = {
    val tacToPrint: Doc = getTacDocument(instructions)
    print(tacToPrint.render(60))
  }

  private def getTacDocument(instructions: List[TAC]): Doc = {
    buildDocument(instructions)
  }

}
