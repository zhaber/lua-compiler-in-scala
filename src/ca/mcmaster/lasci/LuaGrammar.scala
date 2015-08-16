package ca.mcmaster.lasci

import ca.mcmaster.lasci.front_end._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import ca.mcmaster.lasci.middle_end.VarargsType

trait Grammar extends JavaTokenParsers with PackratParsers {

  lazy val keyword: Parser[String] = "break" | "do" | "end" | "else" | "elseif" |
    "function" | "if" | "local" | "nil" | "not" | "or" |
    "repeat" | "return" | "then" | "until" | "while"

  lazy val Name: Parser[Ident] = not(keyword) ~> ident ^^ $ident | failure("Illegal identifier")

  lazy val Number: Parser[Double] = floatingPointNumber ^^ $floatingPointNumber | failure("Illegal float number")

  lazy val StringLiteral: Parser[String] = stringLiteral ^^ $stringLiteral

  lazy val endofline = opt(""";?\s*(--.*)?""".r)

  //chunk ::= {stat [`;´]} [laststat [`;´]]
  lazy val block: PackratParser[List[Statement]] = rep(stat <~ endofline) ~ opt(laststat <~ endofline) ^^ $chunk

  //stat
  lazy val stat: PackratParser[Statement] = positioned(
    //functioncall  
    log(functioncall ^^ $functionCall)("functionCall") |
      //varlist `=´ explist 
      log((varlist <~ """\s*=\s*""".r) ~ explist ^^ $varsAssinment)("varsAssignment") |
      //do block end 
      log("do" ~> block <~ "end" ^^ $do)("do") |
      //while exp do block end 
      log(("while" ~> compoundExp <~ "do") ~ block <~ "end" ^^ $while)("while") |
      //repeat block until exp 
      log("repeat" ~> block ~ ("until" ~> compoundExp) ^^ $repeat)("repeat") |
      //if exp then block {elseif exp then block} [else block] end  
      log("if" ~> compoundExp ~ ("then" ~> block) ~ rep(("elseif" ~> compoundExp) ~ ("then" ~> block) ^^ $elseif) ~
        opt("else" ~> block) <~ "end" ^^ $if)("if") |
      //for Name `=´ exp `,´ exp [`,´ exp] do block end  
      log(("for" ~> (Name <~ "=") ~ (compoundExp <~ ",") ~ compoundExp ~ opt("," ~> compoundExp) <~ "do") ~ block <~ "end" ^^ $for)("for") |
      //for namelist in explist do block end  
      log(("for" ~> (namelist <~ "in") ~ explist <~ "do") ~ block <~ "end" ^^ $foreach)("foreach") |
      //function funcname funcbody 
      log("function" ~> funcname ~ funcbody ^^ $function)("function") |
      //local function Name funcbody  
      log("local function" ~> Name ~ funcbody ^^ $localFunction)("localFunction") |
      //local namelist [`=´ explist]  
      log("local" ~> namelist ~ opt("=" ~> explist) ^^ $localVarDeclarations)("localVarDeclaration") |
      failure("Illegal statement"))

  //laststat ::= return [explist] | break
  lazy val laststat: PackratParser[Statement] = "return" ~> opt(explist) ^^ $return |
    "break" ^^^ $break |
    failure("Illegal last statement")

  //funcname ::= Name {`.´ Name} [`:´ Name]
  lazy val funcname: PackratParser[FunctionName] = rep1sep(Name, ".") ~ opt(":" ~> Name) ^^ $funcname | failure("Illegal function name")

  //varlist ::= var {`,´ var}
  lazy val varlist: PackratParser[List[Variable]] = log(repsep(variable ^^ $variable, ","))("varlist")

  //namelist ::= Name {`,´ Name}
  lazy val namelist: PackratParser[List[Ident]] = rep1sep(Name, ",")

  //explist ::= {exp `,´} exp
  lazy val explist: PackratParser[List[Expression]] = rep1sep(compoundExp, ",")

  /* exp ::= exp binop exp | unop exp | nil | false | true | Number | String | `...´ | function | 
		     prefixexp | tableconstructor */
  lazy val exp: PackratParser[Expression] = log(unop ~ compoundExp ^^ $unOpExp)("unop") |
    log(function ^^ $lambdaExp)("lambdaExp") |
    log("nil" ^^^ $nilExp)("nil") |
    log("false" ^^^ $false)("false") |
    log("true" ^^^ $true)("true") |
    log(Number ^^ $floatNumber)("number") |
    log(StringLiteral ^^ $string)("string") |
    log(VarargsType.argName ^^^ $varArg)("vararg") |
    prefixexp |
    log(tableconstructor ^^ $table)("table") |
    failure("Illegal expression")

  lazy val factorExp: PackratParser[Expression] = exp ~ factorBinop ~ factorExp ^^ $binOpExp | exp

  lazy val compoundExp: PackratParser[Expression] = log(log(factorExp ~ binop ~ compoundExp ^^ $binOpExp)("binop") | factorExp)("compoundExp") |
    failure("Illegal expression")

  lazy val prefixexp: PackratParser[Expression] = log(functioncall | parenthesizedexp | variable)("prefixexp") | failure("Invalid prefix expression")

  lazy val variableSuffix: PackratParser[VariableSuffix] = log("""\[\s*""".r ~> compoundExp <~ """\s*\]""".r ^^ $tableIndexExpression)("tableIndexExpression") |
    log("." ~> Name ^^ $dotIdentExpression)("dotIdentExpression")

  lazy val functionArgsExpression: PackratParser[FunctionArgsExpression] = log(args ^^ $functionArgsExpression)("functionArgsExpression") |
    log(":" ~> Name ~ args ^^ $methodArgsExpression)("methodArgsExpression")

  lazy val variable: PackratParser[VariableExpression] = (Name ^^ $identExpression | parenthesizedexp) ~ rep(rep(functionArgsExpression)
    ~ variableSuffix ^^ $variableSuffix) ^^ $variableExpression

  lazy val functioncall: PackratParser[FunctionCallExpression] = (Name ^^ $identExpression | parenthesizedexp) ~ rep1(rep(variableSuffix)
    ~ functionArgsExpression ^^ $functionCallSuffix) ^^ $functionCallExpression

  lazy val parenthesizedexp: PackratParser[Expression] = log("(" ~> compoundExp <~ ")")("parenthesizedExp")

  //args ::=  `(´ [explist] `)´ | tableconstructor | String 
  lazy val args: PackratParser[List[(Field, Expression)]] = log("(" ~> opt(explist) <~ ")" ^^ $args)("args") |
    tableconstructor |
    StringLiteral ^^ $stringArg |
    failure("Illegal arguments syntax")

  //function ::= function funcbody
  lazy val function: PackratParser[Lambda] = "function" ~> funcbody ^^ $lambda | failure("Illegal function syntax")

  //funcbody ::= `(´ [parlist] `)´ block end
  lazy val funcbody: PackratParser[FunctionBody] = ("(" ~> opt(parlist) <~ ")") ~ block <~ "end" ^^ $functionBody | failure("Illegal function body syntax")

  //parlist ::= namelist [`,´ `...´] | `...´
  lazy val parlist: PackratParser[List[Param]] = namelist ~ opt("," ~> VarargsType.argName) ^^ $parList |
    VarargsType.argName ^^^ $varArgList |
    failure("Illegal parameter list syntax")

  //tableconstructor ::= `{´ [fieldlist] `}´
  lazy val tableconstructor: PackratParser[List[(Field, Expression)]] = "{" ~> repsep(field, fieldsep) <~ "}" | failure("Illegal table syntax")

  //field ::= `[´ exp `]´ `=´ exp | Name `=´ exp | exp
  lazy val field: PackratParser[(Field, Expression)] = ("[" ~> compoundExp <~ "]" <~ "=") ~ compoundExp ^^ $expressionField |
    (Name <~ "=") ~ compoundExp ^^ $namedField |
    compoundExp ^^ $unnamedField | failure("Illegal field syntax")

  //fieldsep ::= `,´ | `;´
  lazy val fieldsep: PackratParser[String] = "," | ";" | failure("Illegal field separator")

  lazy val factorBinop: PackratParser[BinOperation] = Multipl | Div | Power

  /* binop ::= `+´ | `-´ | `%´ | `..´ | 
		       `<´ | `<=´ | `>´ | `>=´ | `==´ | ` ~=´ | and | or */
  lazy val binop: PackratParser[BinOperation] = Plus | Minus <~ not("-") | Mod | Concat |
    LessEq | Less | MoreEq | More | Eq | NotEq | And | Or | failure("Illegal binary operation")

  //unop ::= `-´ | not | `#´
  lazy val unop: PackratParser[UnaryOperation] = Negation <~ not("-") | Not | Length | failure("Illegal unary operator")

  def $ident: String => Ident

  def $floatingPointNumber: String => Double

  def $stringLiteral: String => String

  def $chunk: List[Statement] ~ Option[Statement] => List[Statement]

  def $varsAssinment: List[Variable] ~ List[Expression] => Statement

  def $functionCall: FunctionCallExpression => FunctionCall

  def $do: List[Statement] => Statement

  def $while: Expression ~ List[Statement] => Statement

  def $repeat: List[Statement] ~ Expression => Statement

  def $elseif: Expression ~ List[Statement] => (Expression, List[Statement])

  def $if: Expression ~ List[Statement] ~ List[(Expression, List[Statement])] ~ Option[List[Statement]] => Statement

  def $for: Ident ~ Expression ~ Expression ~ Option[Expression] ~ List[Statement] => Statement

  def $foreach: List[Ident] ~ List[Expression] ~ List[Statement] => Statement

  def $function: FunctionName ~ FunctionBody => Statement

  def $localFunction: Ident ~ FunctionBody => Statement

  def $localVarDeclarations: List[Ident] ~ Option[List[Expression]] => Statement

  def $return: Option[List[Expression]] => Statement

  def $break: Statement

  def $funcname: List[Ident] ~ Option[Ident] => FunctionName

  def $nilExp: Expression

  def $false: Expression

  def $true: Expression

  def $floatNumber: Double => Expression

  def $string: String => Expression

  def $varArg: Expression

  def $lambdaExp: Lambda => Expression

  def $table: List[(Field, Expression)] => Expression

  def $binOpExp: Expression ~ BinOperation ~ Expression => Expression

  def $unOpExp: UnaryOperation ~ Expression => Expression

  def $identExpression: Ident => IdentExpression

  def $tableIndexExpression: Expression => TableIndexExpression

  def $dotIdentExpression: Ident => DotIdentExpression

  def $functionArgsExpression: List[(Field, Expression)] => FunctionArgsExpression

  def $methodArgsExpression: Ident ~ List[(Field, Expression)] => MethodArgsExpression

  def $variableSuffix: List[FunctionArgsExpression] ~ VariableSuffix => List[PrefixExpressionSuffix]

  def $variableExpression: Expression ~ List[List[PrefixExpressionSuffix]] => VariableExpression

  def $variable: VariableExpression => Variable

  def $functionCallExpression: Expression ~ List[List[PrefixExpressionSuffix]] => FunctionCallExpression

  def $functionCallSuffix: List[VariableSuffix] ~ FunctionArgsExpression => List[PrefixExpressionSuffix]

  def $args: Option[List[Expression]] => List[(Field, Expression)]

  def $stringArg: String => List[(Field, Expression)]

  def $lambda: FunctionBody => Lambda

  def $functionBody: Option[List[Param]] ~ List[Statement] => FunctionBody

  def $parList: List[Ident] ~ Option[String] => List[Param]

  def $varArgList: List[Param]

  def $expressionField: Expression ~ Expression => (Field, Expression)

  def $namedField: Ident ~ Expression => (Field, Expression)

  def $unnamedField: Expression => (Field, Expression)

  implicit def $opParser[T <: Operation](op: T): Parser[T]

}