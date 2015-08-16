package ca.mcmaster.lasci
import java.io.OutputStreamWriter
import java.io.FileOutputStream
import scala.util.parsing.combinator._
import ca.mcmaster.lasci.printer.ASTPrinter
import ca.mcmaster.lasci.front_end._
import ca.mcmaster.lasci.middle_end._
import ca.mcmaster.common.Exceptionable.Success
import ca.mcmaster.common.Exceptionable.Failure
import ca.mcmaster.lasci.back_end.CodeGenerator

object Compiler {
	
  def compile(input: String, log: Boolean, progPath: String) {
    val result = LuaParser(input, log)
    if (result.successful) {
      ASTPrinter.print(input, result.get.toString)
      TypeChecker.processBlock(result.get, List[List[Statement]]()) match {
        case None => { 
        	println("Symbol table:")
        	TypeChecker.processBlock(result.get, List[List[Statement]]()) //second path to update function variables
        	println(TypeChecker.symbolTable)
        	val absolutePath = CodeGenerator.generate(result.get, TypeChecker.symbolTable, progPath)
        	println("\nOpen " + absolutePath + " to view the result\nTo execute the code run: llvm-as newprog.ll; lli newprog.bc")
        }
        case Some(Failure(Some(statement))) => { 
        	val statementLine = input.split("\n")(statement.pos.line - 1)
        	println("Type check error in statement at line " + statement.pos.line + ":\n" + statementLine)
        }
        case _ => throw new IllegalStateException()
      }
    } else {
      println(result)
    }
  }

}