package ca.mcmaster.lasci
import java.io.File

object Lasci {

  def run(filePath: String, debug: Boolean) {
    var file = new File("../" + filePath)
    val progPath = "LLVMIR/newprog.ll"
    if (file.exists) {
      val lines = io.Source.fromFile(file).mkString
      Compiler.compile(lines, debug, progPath)
    } else {
      file = new File(filePath)
      if (file.exists) {
    	  val lines = io.Source.fromFile(file).mkString
    	  Compiler.compile(lines, debug, progPath)
      }
      else {
    	  println("File " + filePath + " not found")
      }
    }
  }

  def main(args: Array[String]) {
    args.length match {
      case 1 =>
        run(args(0), false)
      case 2 =>
        run(args(0), true)
      case _ =>
        println("\nUSAGE: ./lasci.sh filepath [-d]\n\n\t-d\tturn on a debug mode\n")
        println("E.g.: ./lasci.sh tests/files/all.lua -d\n")
    }
  }

}