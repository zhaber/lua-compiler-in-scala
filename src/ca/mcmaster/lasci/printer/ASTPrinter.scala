package ca.mcmaster.lasci.printer
import ca.mcmaster.lasci.front_end.Statement
import java.io.OutputStreamWriter
import java.io.FileOutputStream
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.annotation.tailrec

object ASTPrinter {

  val span_open = "<span style=\"color:%s\" onclick=\"$(this).children().toggle(); event.stopPropagation();\">\n"
  val span_close = "</span>"
  val htmlHeader = "<html><head><script src=\"http://code.jquery.com/jquery-1.5.js\"></script></head><body><b><br>Click</b> on block names below to fold/unfold statements<pre>\n" + span_open
  val htmlFooter = span_close + """</pre><script>$("span").hide();$("span:first").show();$("span:first").children().show();</script></body></html>"""

  @tailrec
  def writeResult(out: String => Unit, in: String, i: Int): String = {
    val color = i % 3 match {
      case 0 => "blue"
      case 1 => "black"
      case 2 => "brown"
    }
    if (in.isEmpty) return ""
    in(0) match {
      case '(' => { 
    	  	out('(' + format(span_open, color) + " " * (i + 1)) 
    	  	writeResult(out, in.substring(1), i + 1) 
    	  }
      case ')' => {
    	  out(span_close + "\n" + " " * (i - 1) + ")")
    	  writeResult(out, in.substring(1), i - 1)
      }
      case _ => { 
    	  out(HtmlEscape(in(0).toString))
    	  writeResult(out, in.substring(1), i)
      }
    }
  }

  def print(input: String, program: String) {
    val out: OutputStreamWriter = new OutputStreamWriter(new FileOutputStream("../output.html"), "utf-8");
    out.write("<b>Input:</b> <br><br><pre>" + HtmlEscape(input) + "</pre><br>")
    out.write(htmlHeader)
    writeResult(out.write, program, 0)
    out.write(htmlFooter)
    out.write("<br><b>Raw AST:</b><br><br>" + HtmlEscape(program))
    out.close()
    println("\n-------------------------------------------")
    println("--> OPEN output.html TO VIEW THE RESULT <--")
    println("-------------------------------------------\n")
  }

}