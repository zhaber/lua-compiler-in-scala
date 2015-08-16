package ca.mcmaster.lasci

import ca.mcmaster.lasci.front_end._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.collection.mutable.LinkedHashMap

object Parser {

  def apply(in: String, log: Boolean) = new LuaParser(log).parse(in)

}

class Parser private (val log: Boolean) extends Grammar {

  def parse(in: String) = parseAll(block, in)

  override def log[T](p: => Parser[T])(name) = Parser { in =>
    if (log) {
      println("trying " + name + " at " + in)
    }
    val r = p(in)
    if (log) {
      println(name + " --> " + r)
    }
    r
  }
  
  override def $ident = Ident(_)
  
  override def $stringLiteral = _.init.tail

  override def $floatingPointNumber = _.toDouble

  override def $chunk = {
    case statements ~ laststat =>
      statements ::: (laststat match {
        case None => Nil
        case Some(x) => List(x)
      })
  }

  override def $varsAssinment = { case varlist ~ explist => VarsAssignment(varlist, explist) }

  override def $functionCall = FunctionCall(_)
  
  override def $do = Do(_)

  override def $while = { case exp ~ block => While(exp, block) }

  override def $repeat = { case block ~ exp => Repeat(exp, block) }

  override def $elseif = { case exp ~ block => (exp -> block) }

  override def $if = {
    case ifExp ~ thenBlock ~ elseifExpBlock ~ elseBlock =>
      If(LinkedHashMap(ifExp -> thenBlock) ++ elseifExpBlock, elseBlock match {
        case None => Nil
        case Some(x) => x
      })
  }

  override def $for = {
    case name ~ exp1 ~ exp2 ~ exp3 ~ block =>
      def decrement = exp3 match {
        case None => FloatNumber(1) //default step
        case Some(x) => x
      }
      For(name, exp1, exp2, decrement, block)
  }

  override def $foreach = { case namelist ~ explist ~ block => Foreach(namelist, explist, block) }

  override def $function = { case funcname ~ funcbody => Function(funcname, funcbody) }

  override def $localFunction = { case funcname ~ funcbody => LocalFunction(funcname, funcbody) }

  override def $localVarDeclarations = {
    case namelist ~ explist =>
      def exps = explist match {
        case None => Nil
        case Some(explist) => explist
      }
      LocalVarDeclarations(namelist, exps)
  }

  override def $return = {
    _ match {
      case None => Return(Nil)
      case Some(list) => Return(list)
    }
  }

  override def $break = Break

  override def $funcname = {
    case name ~ method =>
      method match {
        case None => FunctionName(name)
        case Some(method) => MethodName(name, method)
      }
  }

  override def $identExpression = IdentExpression(_)

  override def $tableIndexExpression = TableIndexExpression(_)

  override def $dotIdentExpression = DotIdentExpression(_)

  override def $nilExp = NilExpression

  override def $false = False

  override def $true = True

  override def $floatNumber = FloatNumber(_)

  override def $string = StringExpression(_)

  override def $varArg = VarArgExpression

  override def $lambdaExp = LambdaExpression(_)

  override def $table = TableExpression(_)

  override def $binOpExp = {
    case exp1 ~ binop ~ exp2 => BinOpExpression(exp1, binop, exp2)
  }

  override def $unOpExp = {
    case unop ~ exp => UnOpExpression(unop, exp)
  }

  override def $variable = Variable(_)

  override def $functionCallExpression = {
    case expression ~ suffix =>
      FunctionCallExpression(expression :: suffix.flatten.init, suffix.flatten.last.asInstanceOf[FunctionArgsExpression])
  }
  
  override def $functionCallSuffix = { case variableSuffixes ~ functionArgsExpression => variableSuffixes ::: List(functionArgsExpression) }

  override def $variableExpression = {
    case variablePrefix ~ suffix =>
      VariableExpression(variablePrefix :: suffix.flatten)
  }

  override def $variableSuffix = { case functionArgsExpressiones ~ variableSuffix => functionArgsExpressiones ::: List(variableSuffix) }

  override def $functionArgsExpression = { case args => FunctionArgsExpression(args) }

  override def $methodArgsExpression = { case name ~ args => MethodArgsExpression(name, args) }

  override def $args = {
    _ match {
      case None => List[(Field, Expression)]()
      case Some(explist) => List[(Field, Expression)]() ::: explist.map(UnnamedField -> _)
    }
  }

  override def $stringArg = string => List[(Field, Expression)](UnnamedField -> StringExpression(string))

  override def $lambda = Lambda(_)

  override def $functionBody = {
    case paramlist ~ block =>
      def params = paramlist match {
        case None => Nil
        case Some(params) => params
      }
      FunctionBody(params, block)
  }

  override def $parList = {
    case namelist ~ vararg =>
      namelist.map(NamedParam(_)) ::: (vararg match {
        case None => Nil
        case _ => List(VarArg)
      })
  }

  override def $varArgList = List(VarArg)

  override def $expressionField = { case exp1 ~ exp2 => (ExpressionField(exp1), exp2) }

  override def $namedField = { case name ~ exp => (NamedField(name), exp) }

  override def $unnamedField = (UnnamedField, _)

  override implicit def $opParser[T <: Operation](op: T): Parser[T] = new Parser[T] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      var i = 0
      var j = start
      val s = op.toString
      while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
        i += 1
        j += 1
      }
      if (i == s.length)
        Success(op, in.drop(j - offset))
      else 
        Failure("`"+s+"' expected but `"+in.first+"' found", in.drop(start - offset))
    }
  }
  
}