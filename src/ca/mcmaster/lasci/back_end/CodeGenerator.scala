package ca.mcmaster.lasci.back_end
import scala.collection.mutable.HashMap
import ca.mcmaster.lasci.front_end._
import ca.mcmaster.lasci._
import ca.mcmaster.lasci.ArithmeticOperation
import ca.mcmaster.lasci.middle_end._
import org.slem.IRTree._
import org.slem.IRTreeEncoder
import org.kiama.util.StringEmitter
import scala.collection.mutable.ListBuffer
import java.io.File

object CodeGenerator {
  def generate(program: List[Statement], symbolTable: SymbolTable, progPath: String): String = {
    new CodeGenerator(symbolTable).generateCode(program, progPath)
  }
}

class CodeGenerator(symbolTable: SymbolTable) {

  var blockList: List[L_Block] = List()
  var currentBlockInstructions: List[L_Instruction] = List()
  var currentBlockLabel = L_Label("entry")

  val globals = ListBuffer[L_Global]()
  val localAllocators = HashMap[String, ListBuffer[L_Instruction]]()

  val argc = L_Argument(L_IntType(32), argName = "argc")
  val argv = L_Argument(L_PointerType(L_PointerType(L_IntType(8))), argName = "argv")
  var functionDepth = 0

  var functionBlockList: List[L_Block] = List()
  var functionCurrentBlockInstructions: List[L_Instruction] = List()
  var functionCurrentBlockLabel = L_Label("functionEntry")

  private def isTrue(value: L_Value): L_ICMP = {
    L_ICmpNE(value, L_Boolean(false))
  }

  private def isFalse(value: L_Value): L_ICMP = {
    L_ICmpEQ(value, L_Boolean(false))
  }

  private val formatDouble = L_GlobalVariable(StringType.create("%f\\0A"), linkage = "private", isConstant = true, alignment = 1)
  private val formatString = L_GlobalVariable(StringType.create("%s\\0A"), linkage = "private", isConstant = true, alignment = 1)
  private val formatConcatString = L_GlobalVariable(StringType.create("%s%s\\0A"), linkage = "private", isConstant = true, alignment = 1)
  private val printfDef = L_FunctionDeclaration(L_IntType(32), arguments = List(L_PointerType(L_IntType(8)), L_VarArgsType()), funcName = "printf")
  private val snprintfDef = L_FunctionDeclaration(L_IntType(32), arguments = List(L_PointerType(L_IntType(8)), L_IntType(32),
    L_PointerType(L_IntType(8)), L_VarArgsType()), funcName = "snprintf")
  private val vaStartDef = L_FunctionDeclaration(L_VoidType(), arguments = List(L_PointerType(L_IntType(8))), funcName = "llvm.va_start")
  private val vaEndDef = L_FunctionDeclaration(L_VoidType(), arguments = List(L_PointerType(L_IntType(8))), funcName = "llvm.va_end")

  private val imports = List(formatDouble, formatString, formatConcatString, printfDef, snprintfDef, vaStartDef, vaEndDef)

  def printResult() = {
    symbolTable.get("result", List[List[Statement]]()) match {
      case Some(resultSymbol) => {
        val format = resultSymbol.value.asInstanceOf[L_GlobalVariable].value match {
          case n: L_Double => {
            formatDouble
          }
          case string: L_NullPointer => {
            formatString
          }
        }
        val printfDoubleFormatPointer = L_GetElementPtr(format -> resultType, format, List(0, 0))
        val result = L_Load(resultSymbol.value -> resultType, resultSymbol.value)
        val printfCall = L_Call(L_IntType(32), printfDef, List(printfDoubleFormatPointer, result))
        addInstructions(List(result, printfDoubleFormatPointer, printfCall))
      }
      case None => {}
    }
  }

  def emitProgram(program: L_Program, progPath: String): String = {
    val e = new IRTreeEncoder(new StringEmitter())
    val file = new File(progPath)
    val parent = new File(file.getParent());
    parent.mkdirs()
    file.createNewFile()
    e.encodeTree(program)
    file.getAbsolutePath()
  }

  def generateCode(program: List[Statement], progPath: String): String = {
    blockList = List()
    currentBlockLabel = L_Label("entry")
    currentBlockInstructions = List()
    val entryLabel = L_Label("lascientry")

    currentBlockLabel = entryLabel
    for (symbol <- symbolTable.symbols()) {
      symbol.ltypes.head match {
        case FunctionType(_, _) => localAllocators.put(symbol.name, ListBuffer[L_Instruction]())
        case _ => {}
      }
    }
    for (symbol <- symbolTable.symbols()) {
      if (!symbol.ltypes.head.isInstanceOf[FunctionType])
        symbol.scope match {
          case Global => if (!symbol.ltypes.head.isInstanceOf[FunctionType]) globals += Allocator.allocateGlobal(symbol)
          case Local(block) => {
            var found = false
            for (statement <- program) {
              statement match {
                case function@Function(_, body) =>
                  if (body.block == block) {
                    symbolTable.get(function.name, List()).get.ltypes.head match {
                      case functionType@FunctionType(_, args) =>
                        args.find(_.name == symbol.name) match {
                          case Some(arg) => symbol.value = L_Argument(arg.ltype.toL_Type())
                          case None => localAllocators.get(function.name).get += Allocator.allocateLocal(symbol)
                        }
                      case _ => throw new IllegalStateException("Function " + function.name + " excpected")
                    }
                    found = true
                  }
                case _ => {}
              }
            }
            if (!found) {
              globals += Allocator.allocateGlobal(symbol) //global variable
            }
          }
        }
    }

    for (s <- program) {
      generateStatement(s, List[List[Statement]]())
    }
    printResult()
    addInstruction(L_Ret(0))

    val args = List(argc, argv)
    val mainFunc = L_FunctionDefinition(L_IntType(32), blockList, funcName = "main", arguments = args)
    val mainmodule = L_Module(List(mainFunc) ::: globals.toList ::: imports)
    val targettree = L_Program(List(mainmodule))

    emitProgram(targettree, progPath)
  }

  def generateExpression(exp: Expression, parentBlocks: List[List[Statement]]): L_Value = {
    exp match {
      case NilExpression => NilType.create()
      case True => BooleanType.create(true)
      case False => BooleanType.create(false)
      case FloatNumber(number) => NumberType.create(number)
      case StringExpression(string) => generateString(string)
      case LambdaExpression(lambda) => throw new UnsupportedOperationException("Lambda functions are not supported")
      case TableExpression(fields) => generateTable(fields, parentBlocks)
      case UnOpExpression(operation, exp) => generateUnaryOperation(operation, exp, parentBlocks)
      case VarArgExpression => throw new UnsupportedOperationException("... is not supported, try arg[1] instead")
      case BinOpExpression(expLeft, operation, expRight) => generateBinOperation(operation, expLeft, expRight, parentBlocks)
      case IdentExpression(ident) => generateIdent(ident, parentBlocks)
      case functionCallExp@FunctionCallExpression(_, _) => generateFunctionCall(functionCallExp, parentBlocks)
      case VariableExpression(expressions) => generateVariable(expressions, parentBlocks)
      case TableIndexExpression(expression) => throw new IllegalStateException("Table indexes are processed by variable generator")
      case DotIdentExpression(_) => throw new UnsupportedOperationException("Dot expressions are not supported")
      case FunctionArgsExpression(argsExpression) => generateFields(argsExpression, parentBlocks)
    }
  }

  def generateString(string: String): L_Value = {
    val globalVariable = L_GlobalVariable(StringType.create(string), linkage = "private", isConstant = true, alignment = 1)
    globals += globalVariable
    val getElementPtr = L_GetElementPtr(globalVariable -> resultType, globalVariable, List(0, 0))
    addInstruction(getElementPtr)
    getElementPtr
  }

  def generateStatement(statement: Statement, parentBlocks: List[List[Statement]]): Unit = {
    statement match {
      case varAssignment@VarsAssignment(_, _) => generateVarsAssignment(varAssignment, parentBlocks)
      case Break => List[L_Instruction](L_Br(currentBlockLabel))
      case Do(statements) => generateBlock(statements, parentBlocks)
      case forStatement@For(_, _, _, _, _) => generateFor(forStatement, parentBlocks)
      case foreachStatement@Foreach(_, _, _) => generateForEach(foreachStatement, parentBlocks)
      case ifStatement@If(_, _) => generateIf(ifStatement, parentBlocks)
      case repeatStatement@Repeat(_, _) => generateRepeat(repeatStatement, parentBlocks)
      case localVarDeclarations@LocalVarDeclarations(_, _) => generateLocalVarDeclarations(localVarDeclarations, parentBlocks)
      case whileStatement@While(_, _) => generateWhile(whileStatement, parentBlocks)
      case function@Function(_, _) => generateGlobalFunction(function, List[List[Statement]]())
      case localFunction@LocalFunction(_, _) => generateLocalFunction(localFunction, parentBlocks)
      case FunctionCall(functionCallExp) => generateFunctionCall(functionCallExp, parentBlocks)
      case returnStatement@Return(expressions) => throw new IllegalStateException()
    }
  }

  private def addInstruction(instr: L_Instruction): Unit = {
    if (functionDepth == 0) {
      functionBlockList = List()
      functionCurrentBlockInstructions = List()
      functionCurrentBlockLabel = L_Label("functionEntry")
      instr match {
        case i: L_TerminatorInstruction => {
          blockList = blockList ::: List(L_Block(currentBlockInstructions, i, label = currentBlockLabel))
          currentBlockInstructions = List()
          currentBlockLabel = L_Label("")
        }
        case _ => {
          currentBlockInstructions = currentBlockInstructions ::: List(instr)
        }
      }
    } else {
      instr match {
        case i: L_TerminatorInstruction => {
          functionBlockList = functionBlockList ::: List(L_Block(functionCurrentBlockInstructions, i, label = functionCurrentBlockLabel))
          functionCurrentBlockInstructions = List()
          functionCurrentBlockLabel = L_Label("")
        }
        case _ => {
          functionCurrentBlockInstructions = functionCurrentBlockInstructions ::: List(instr)
        }
      }
    }
  }

  private def addInstructions(instrs: List[L_Instruction]): Unit = {
    for (instr <- instrs) {
      addInstruction(instr)
    }
  }

  def generateUnaryOperation(operation: UnaryOperation, exp: Expression, parentBlocks: List[List[Statement]]): L_Value = {
    val expval = generateExpression(exp, parentBlocks)
    operation match {
      case Negation => {
        val out = L_FSub(0.0, expval)
        addInstruction(out)
        out
      }
      case Not => {
        val isEqual = L_ICmpEQ(expval, L_Boolean(false))
        addInstruction(isEqual)
        isEqual
      }
      case Length => expval match {
    	  case L_GetElementPtr(L_PointerType(L_ArrayType(length,_)),_,_,_) => L_Double("" + (length - 1).toDouble)
    	  case L_Load(L_PointerType(L_VectorType(length,_)),_,_,_) => L_Double("" + length.toDouble)
    	  case L_Load(L_PointerType(L_StructureType(ltypes)),_,_,_) => L_Double("" + ltypes.size.toDouble)
      }
    }
  }

  def generateVariable(expressions: List[Expression], parentBlocks: List[List[Statement]]): L_Value = {
    if (expressions.size == 1 && expressions.head.isInstanceOf[IdentExpression]) {
      val ident = expressions.head.asInstanceOf[IdentExpression].ident
      generateIdent(ident, parentBlocks)
    } else if (expressions.size == 2 && expressions(1).isInstanceOf[TableIndexExpression]) {
      val name = expressions.head.asInstanceOf[IdentExpression].ident.name
      if (name != VarargsType.variableName) {
        symbolTable.get(name, parentBlocks).get.ltypes.head match {
          case VectorType(_, _) => {
            val tablePointer = symbolTable.get(name, parentBlocks).get.value
            val table = L_Load(tablePointer -> resultType, tablePointer)
            addInstruction(table)
            val index = generateExpression(expressions(1).asInstanceOf[TableIndexExpression].index, parentBlocks)
            val extractElement = L_ExtractElement(table, generateRound(index))
            addInstruction(extractElement)
            extractElement
          }
          case TableType(ltypes) => {
            val tablePointer = symbolTable.get(name, parentBlocks).get.value
            val table = L_Load(tablePointer -> resultType, tablePointer)
            addInstruction(table)
            val key = expressions(1).asInstanceOf[TableIndexExpression].index.asInstanceOf[StringExpression].string
            val extractValue = L_ExtractValue(table, List(ltypes.keys.toList.indexOf(StringType.create(key))))
            addInstruction(extractValue)
            extractValue
          }
          case _ => throw new IllegalStateException()
        }
      } else {
        generateVararg(symbolTable.get(VarargsType.argName, parentBlocks).get.ltypes.head.toL_Type())
      }
    } else {
      throw new UnsupportedOperationException("Dot variables are not supported")
    }
  }

  def generateFunctionCall(functionCallExp: FunctionCallExpression, parentBlocks: List[List[Statement]]): L_Call = {
    val expressions = functionCallExp.exps
    val argsExp = functionCallExp.argsExp
    val args = argsExp.args.map(functionArg => valueToArgument(generateExpression(functionArg._2, parentBlocks))).toList
    if (expressions.size == 1 && expressions.head.isInstanceOf[IdentExpression] && symbolTable.get(expressions.head.asInstanceOf[IdentExpression].ident.name, parentBlocks).isDefined) {
      val function = symbolTable.get(expressions.head.asInstanceOf[IdentExpression].ident.name, parentBlocks).get
      val name = function.name
      val ltype = function.ltypes.head
      val functionDefinition = symbolTable.get(name, parentBlocks).get.value.asInstanceOf[L_FunctionDefinition]
      val call = L_Call(functionDefinition.returnType, functionDefinition, args)
      addInstruction(call)
      call
    } else {
      throw new UnsupportedOperationException("Methods are not supported")
    }
  }

  def generateIdent(ident: Ident, parentBlocks: List[List[Statement]]): L_Value = {
    symbolTable.get(ident.name, parentBlocks) match {
      case Some(symbol) => {
        val symbol = symbolTable.get(ident.name, parentBlocks).get
        symbol.value match {
          case arg: L_Argument => symbol.value
          case _ => {
            val loading = L_Load(L_PointerType(symbol.ltypes.head.toL_Type()), symbol.value)
            addInstruction(loading)
            loading
          }
        }
      }
      case None => NilType.create()
    }
  }

  def generateTable(fields: List[(Field, Expression)], parentBlocks: List[List[Statement]]): L_Value = {
    val values = fields.map(field => generateExpression(field._2, parentBlocks))
    val ltype = fields.head._1 match {
      case NamedField(_) => {
    	  L_Structure(values) -> resultType 
      }
      case UnnamedField => {
    	  L_Vector(values) -> resultType
      }
      case _ => throw new IllegalStateException()
    }
    val table = L_Alloca(ltype)
    addInstruction(table)
    for (i <- 0 until values.size) {
      val valuePtr = L_GetElementPtr(L_PointerType(ltype), table, List(0, i))
      addInstruction(valuePtr)
      addInstruction(L_Store(values(i), valuePtr, alignment = 4))
    }
    val loadTable = L_Load(L_PointerType(ltype), table)
    addInstruction(loadTable)
    loadTable
  }

  def generateFields(fields: List[(Field, Expression)], parentBlocks: List[List[Statement]]): L_Value = {
    throw new UnsupportedOperationException("Fields are not supported")
  }

  def generateFor(forStatement: For, parentBlocks: List[List[Statement]]): Unit = {
    val indval = generateExpression(forStatement.exp1, parentBlocks)
    val step = generateExpression(forStatement.exp3, parentBlocks)
    val idn = forStatement.i.name
    val forStatementParentBlocks = parentBlocks ::: List(forStatement.block)
    val ptr = symbolTable.get(idn, forStatementParentBlocks).get.value
    val storeInstr = L_Store(indval, ptr)
    addInstruction(storeInstr)

    var forblock = L_Label("")
    var stmtblock = L_Label("")
    var exitblock = L_Label("")

    addInstruction(L_Br(forblock))

    currentBlockLabel = forblock

    val maxvalue = generateExpression(forStatement.exp2, forStatementParentBlocks)

    var ptr1 = symbolTable.get(idn, forStatementParentBlocks).get.value

    val currentIndexVal = L_Load(ptr -> resultType, ptr1)
    addInstruction(currentIndexVal)

    val condbool = L_FCmpULE(currentIndexVal, maxvalue)
    addInstruction(condbool)

    addInstruction(L_BrCond(condbool, stmtblock, exitblock))

    currentBlockLabel = stmtblock
    for (s <- forStatement.block) {
      generateStatement(s, forStatementParentBlocks)
    }
    var ptr2 = symbolTable.get(idn, forStatementParentBlocks).get.value
    val oldIndexVal = L_Load(ptr -> resultType, ptr2)
    val newIndexVal = L_FAdd(oldIndexVal, step)
    val storeNewIndex = L_Store(newIndexVal, ptr2)
    addInstructions(List(oldIndexVal, newIndexVal, storeNewIndex))
    addInstruction(L_Br(forblock))
    currentBlockLabel = exitblock

  }

  def generateForEach(forStatement: Foreach, parentBlocks: List[List[Statement]]): List[L_Instruction] = {
    throw new UnsupportedOperationException("Foreach loops are not supported")
  }

  def generateRepeat(repeatStatement: Repeat, parentBlocks: List[List[Statement]]): Unit = {
    val repeatblock = L_Label("")
    val exitblock = L_Label("")

    addInstruction(L_Br(repeatblock))

    currentBlockLabel = repeatblock
    for (s <- repeatStatement.block) {
      generateStatement(s, parentBlocks)
    }

    val expvalue = generateExpression(repeatStatement.exp, parentBlocks)
    val condbool = isFalse(expvalue)
    val condbr = L_BrCond(condbool, repeatblock, exitblock)
    addInstructions(List(condbool, condbr))

    currentBlockLabel = exitblock
  }

  def generateWhile(whileStatement: While, parentBlocks: List[List[Statement]]): Unit = {
    val whileblock = L_Label("")
    val stmtblock = L_Label("")
    val exitblock = L_Label("")

    addInstruction(L_Br(whileblock))

    currentBlockLabel = whileblock
    val expvalue = generateExpression(whileStatement.exp, parentBlocks)
    val condbool = isTrue(expvalue)
    val condbr = L_BrCond(condbool, stmtblock, exitblock)

    addInstructions(List(condbool, condbr))

    currentBlockLabel = stmtblock
    for (s <- whileStatement.block) {
      generateStatement(s, parentBlocks)
    }
    addInstruction(L_Br(whileblock))

    currentBlockLabel = exitblock
  }

  def generateFunction(name: String, body: FunctionBody, parentBlocks: List[List[Statement]]): Unit = {
    functionDepth += 1
    functionCurrentBlockInstructions = localAllocators(name).toList
    generateFunctionBlock(body.block, body.block :: List[List[Statement]]())
    val functionSymbol = symbolTable.get(name, parentBlocks).get
    val args = ListBuffer[L_Argument]()
    for (arg <- functionSymbol.ltypes.head.asInstanceOf[FunctionType].args) {
      for (symbol <- symbolTable.symbols()) {
        if (symbol.name == arg.name) {
          symbol.scope match {
            case Global => {}
            case Local(block) =>
              if (body.block == block) {
                if (symbol.name != VarargsType.argName) {
                  args += symbol.value.asInstanceOf[L_Argument]
                } else {
                  args += L_Argument(L_VarArgsType())
                }
              }
          }
        }
      }
    }
    val functionDefinition = new FunctionDefinition(name, args.toList, functionBlockList)
    val function = functionSymbol.ltypes.head.asInstanceOf[FunctionType].create(functionDefinition)
    globals += function
    functionSymbol.value = function
    functionDepth -= 1
  }

  def generateGlobalFunction(function: Function, parentBlocks: List[List[Statement]]): Unit = {
    generateFunction(function.name, function.body, parentBlocks)
  }

  def generateLocalFunction(function: LocalFunction, parentBlocks: List[List[Statement]]): Unit = {
    generateFunction(function.ident.name, function.body, parentBlocks)
  }

  def generateIf(ifStatement: If, parentBlocks: List[List[Statement]]): Unit = {
    val exitBlock = L_Label("")

    for ((exp, block) <- ifStatement.ifExpBlocks) {
      val thenBlock = L_Label("")
      val elseifBlock = L_Label("")

      val expValue = generateExpression(exp, parentBlocks)
      val condBool = isTrue(expValue)
      val condBranch = L_BrCond(condBool, thenBlock, elseifBlock)
      addInstructions(List(condBool, condBranch))

      currentBlockLabel = thenBlock
      for (s <- block) {
        generateStatement(s, parentBlocks)
      }

      val exit = L_Br(exitBlock)
      addInstruction(exit)
      currentBlockLabel = elseifBlock
    }

    for (s <- ifStatement.elseBlock) {
      generateStatement(s, parentBlocks)
    }

    val exit = L_Br(exitBlock)
    addInstruction(exit)
    currentBlockLabel = exitBlock
  }

  def generateLocalVarDeclarations(localVarDeclarations: LocalVarDeclarations, parentBlocks: List[List[Statement]]): Unit = {
    for (variable <- localVarDeclarations.idents.zip(localVarDeclarations.exps)) {
      val value = generateExpression(variable._2, parentBlocks)
      val symbol = symbolTable.get(variable._1.name, parentBlocks).get
      addInstruction(L_Store(value, symbol.value))
    }
  }

  private def generateRound(lvalue: L_Value) = {
    val round = L_FPToSI(lvalue, L_IntType(32))
    addInstruction(round)
    round
  }

  def generateVarsAssignment(varsAssignment: VarsAssignment, parentBlocks: List[List[Statement]]): Unit = {
    varsAssignment.values.head match {
      case VarArgExpression => {
        throw new UnsupportedOperationException("Varargs are not supported")
      }
      case _ => {
        for ((variable, value) <- varsAssignment.variables.zip(varsAssignment.values)) {
          val lvalue = generateExpression(value, parentBlocks)
          val varExpressions = variable.varExpression.exps
          if (varExpressions.size == 1) {
            varExpressions.head match {
              case IdentExpression(ident) => {
                val symbol = symbolTable.get(ident.name, parentBlocks).get
                addInstruction(L_Store(lvalue, symbol.value))
              }
              case _ => throw new UnsupportedOperationException("Tables are not supported")
            }
          } else if (varExpressions.size == 2 && varExpressions(1).isInstanceOf[TableIndexExpression]) {
            val tableSymbol = symbolTable.get(varExpressions.head.asInstanceOf[IdentExpression].ident.name, parentBlocks).get
            val tablePointer = tableSymbol.value
            val table = L_Load(tablePointer -> resultType, tablePointer)
            val indexExp = varExpressions(1).asInstanceOf[TableIndexExpression].index
            addInstruction(table)
            val insertInstruction = tableSymbol.ltypes.head match {
              case VectorType(_, _) => {
                val index = generateRound(generateExpression(indexExp, parentBlocks))
                L_InsertElement(table, lvalue, index)
              }
              case TableType(ltypes) => {
                L_InsertValue(table, lvalue, ltypes.keys.toList.indexOf(StringType.create(indexExp.asInstanceOf[StringExpression].string)))
              }
              case _ => throw new IllegalStateException()
            }
            addInstruction(insertInstruction)
            addInstruction(L_Store(insertInstruction, tablePointer))
            insertInstruction

          }
        }
      }
    }
  }

  def generateFunctionBlock(block: List[Statement], parentBlocks: List[List[Statement]]): Unit = {
    generateBlock(block.init, parentBlocks)
    val ret = if (block.last.isInstanceOf[Return]) {
      val expressions = block.last.asInstanceOf[Return].expressions
      expressions.size match {
        case 0 => L_Ret(L_VoidType())
        case 1 => L_Ret(generateExpression(expressions.head, parentBlocks))
        case _ => {
          val values = expressions.map(generateExpression(_, parentBlocks))
          L_Ret(TupleType(List[Set[LType]]()).create(values))
        }
      }
    } else {
      generateStatement(block.last, parentBlocks)
      L_Ret(L_VoidType())
    }
    addInstruction(ret)
  }

  def generateBlock(block: List[Statement], parentBlocks: List[List[Statement]]): Unit = {
    block.map(generateStatement(_, parentBlocks ::: List(block)))
  }

  def generateBinOperation(operation: BinOperation, expLeft: Expression, expRight: Expression, parentBlocks: List[List[Statement]]): L_Value = {
    val lExpressionLeft = generateExpression(expLeft, parentBlocks)
    val lExpressionRight = generateExpression(expRight, parentBlocks)
    operation match {
      case operation@ArithmeticOperation() => {
        val out = operation match {
          case Minus => L_FSub
          case Plus => L_FAdd
          case Multipl => L_FMul
          case Div => L_FDiv
          case Power => throw new UnsupportedOperationException("Power is not supported")
          case Mod => L_FRem
        }
        val instruction = out(lExpressionLeft, lExpressionRight)
        addInstruction(instruction)
        instruction
      }
      case operation@BooleanOperation() => {
        val leftBool = isTrue(lExpressionLeft)
        val rightBool = isTrue(lExpressionRight)
        val l_operation = operation match {
          case And => L_And(leftBool, rightBool)
          case Or => L_Or(leftBool, rightBool)
        }
        addInstructions(List(leftBool, rightBool, l_operation))
        l_operation
      }
      case operation@CompareOperation() => {
        val cmp = operation match {
          case Less => {
            L_FCmpULT(lExpressionLeft, lExpressionRight)
          }
          case LessEq => {
            L_FCmpULE(lExpressionLeft, lExpressionRight)
          }
          case More => {
            L_FCmpUGT(lExpressionLeft, lExpressionRight)
          }
          case MoreEq => {
            L_FCmpUGE(lExpressionLeft, lExpressionRight)
          }
        }
        addInstruction(cmp)
        cmp
      }
      case operation@Concat => {
        val snprintfFormatPointer = L_GetElementPtr(formatConcatString -> resultType, formatConcatString, List(0, 0))
        val resultSize = getStringSize(lExpressionLeft) + getStringSize(lExpressionRight) - 1
        val result = Allocator.allocateString(resultSize)
        val resultPointer = L_GetElementPtr(result -> resultType, result, List(0, 0))
        val snprintfCall = L_Call(L_IntType(32), snprintfDef, List(resultPointer, L_Int(32, resultSize), snprintfFormatPointer, lExpressionLeft, lExpressionRight))
        addInstructions(List(snprintfFormatPointer, result, resultPointer, snprintfCall))
        resultPointer
      }
      case operation@EqualityOperation() => {
        val l_type = lExpressionLeft -> resultType
        val isEqual = operation match {
          case Eq => {
            l_type match {
              case L_DoubleType() => L_FCmpUEQ(lExpressionLeft, lExpressionRight)
              case L_IntType(1) => L_ICmpEQ(lExpressionLeft, lExpressionRight)
              case L_VoidType() => throw new IllegalStateException("Void type is illegal for equal operation")
              case _ => throw new UnsupportedOperationException("Type " + l_type + " is not supported for equal operation")
            }
          }
          case NotEq => {
            l_type match {
              case L_DoubleType() => L_FCmpUNE(lExpressionLeft, lExpressionRight)
              case L_IntType(1) => L_ICmpNE(lExpressionLeft, lExpressionRight)
              case L_VoidType() => throw new IllegalStateException("Void type is illegal for equal operation")
              case _ => throw new UnsupportedOperationException("Type " + l_type + " is not supported for not equal operation")
            }
          }
        }
        addInstructions(List(isEqual))
        isEqual
      }
    }
  }

  private def getStringSize(string: L_Value) = string.asInstanceOf[L_GetElementPtr].pty.asInstanceOf[L_PointerType].pointer.asInstanceOf[L_ArrayType].numElements

  def generateVararg(ltype: L_Type): L_Value = {
    val memtmp = L_Alloca(L_PointerType(L_IntType(8)))
    val memtmpBitcastStart = L_Bitcast(memtmp, L_PointerType(L_IntType(8)))
    val vaStart = L_Call(L_VoidType(), vaStartDef, List(memtmpBitcastStart))
    val memtmpLoad = L_Load(L_PointerType(L_PointerType(L_IntType(8))), memtmp, alignment = 4)
    val memtmpPointer = L_GetElementPtr(memtmpLoad -> resultType, memtmpLoad, List(8))
    val memtmpStore = L_Store(memtmpPointer, memtmp, alignment = 4)
    val valueBitcast = L_Bitcast(memtmpLoad, L_PointerType(ltype))
    val valueLoad = L_Load(L_PointerType(ltype), valueBitcast, alignment = 4)
    val memtmpBitcastEnd = L_Bitcast(memtmp, L_PointerType(L_IntType(8)))
    val vaEnd = L_Call(L_VoidType(), vaEndDef, List(memtmpBitcastEnd))
    addInstructions(List(memtmp, memtmpBitcastStart, vaStart, memtmpLoad, memtmpPointer, memtmpStore,
      valueBitcast, valueLoad, memtmpBitcastEnd, vaEnd))
    valueLoad
  }
}
