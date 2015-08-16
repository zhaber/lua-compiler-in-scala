package ca.mcmaster.lasci.middle_end
import ca.mcmaster.lasci.front_end._
import ca.mcmaster.lasci._
import ca.mcmaster.common.Exceptionable._
import ca.mcmaster.lasci.ArithmeticOperation
import scala.collection.mutable.ListBuffer
import org.slem.IRTree._
import scala.collection.mutable.LinkedHashMap

object TypeChecker {

  type SuccessTypeCheck = Set[LType]
  type FailureTypeCheck = Option[Statement]
  type TypeCheckResult = Exceptionable[SuccessTypeCheck, FailureTypeCheck]
  type FailureStatement = Option[Failure[SuccessTypeCheck, FailureTypeCheck]]

  val symbolTable = new SymbolTable()
  val allLTypes: Set[LType] = Set[LType](NumberType, StringType, BooleanType, TableType(LinkedHashMap()), FunctionType(allLTypes, List()), NilType, VoidType)

  implicit def ltypeToSuccess(lType: LType): Success[SuccessTypeCheck, FailureTypeCheck] = Success(Set(lType))
  implicit def statementToFailure[T <: Statement](statement: T): FailureStatement = Some(Failure(Some(statement)))

  def processExpression(exp: Expression, parentBlocks: List[List[Statement]]): TypeCheckResult = {
    exp match {
      case NilExpression => NilType
      case True => BooleanType
      case False => BooleanType
      case FloatNumber(_) => NumberType
      case StringExpression(_) => StringType
      case LambdaExpression(lambda) => processLambda(lambda, parentBlocks)
      case TableExpression(fields) => processTable(fields, parentBlocks)
      case UnOpExpression(operation, exp) => processUnaryOperation(operation, exp, parentBlocks)
      case VarArgExpression => VarargsType
      case BinOpExpression(expLeft, operation, expRight) => processBinOperation(operation, expLeft, expRight, parentBlocks)
      case IdentExpression(ident) => processIdent(ident, parentBlocks)
      case functionCallExp: FunctionCallExpression => processFunctionCall(functionCallExp, parentBlocks)
      case VariableExpression(expressions) => processVariable(expressions, parentBlocks)
      case TableIndexExpression(expression) => processExpression(expression, parentBlocks)
      case exp: DotIdentExpression => Success(Set[LType]())
      case FunctionArgsExpression(argsExpression) => processFields(argsExpression, parentBlocks)
    }
  }

  def processStatement(statement: Statement, parentBlocks: List[List[Statement]]): FailureStatement = {
    statement match {
      case varAssignment: VarsAssignment => processVarsAssignment(varAssignment, parentBlocks)
      case Break => None
      case Do(statements) => processBlock(statements, parentBlocks)
      case forStatement: For => processFor(forStatement, parentBlocks)
      case foreachStatement: Foreach => processForEach(foreachStatement, parentBlocks)
      case ifStatement: If => processIf(ifStatement, parentBlocks)
      case repeatStatement: Repeat => processRepeat(repeatStatement, parentBlocks)
      case localVarDeclarations: LocalVarDeclarations => processLocalVarDeclarations(localVarDeclarations, parentBlocks)
      case whileStatement: While => processWhile(whileStatement, parentBlocks)
      case function: Function => processFunction(function, parentBlocks)
      case localFunction: LocalFunction => processLocalFunction(localFunction, parentBlocks)
      case FunctionCall(functionCallExp) =>
        processFunctionCall(functionCallExp, parentBlocks) match {
          case Success(_) => None
          case Failure(Some(failure)) => failure
          case Failure(None) => statement
        }
      case returnStatement@Return(expressions) => None
    }
  }

  def processUnaryOperation(operation: UnaryOperation, exp: Expression, parentBlocks: List[List[Statement]]): TypeCheckResult = {
    val lTypes = processExpression(exp, parentBlocks)
    lTypes match {
      case Success(ltypes) => {
        val unaryOperationLTypes = ltypes.map(
          _ match {
            case NumberType =>
              operation match {
                case Negation => Some(NumberType)
                case Not => None
                case Length => None
              }
            case StringType =>
              operation match {
                case Negation => None
                case Not => None
                case Length => Some(NumberType)
              }
            case BooleanType =>
              operation match {
                case Negation => None
                case Not => Some(BooleanType)
                case Length => None
              }
            case v: TableType =>
              operation match {
                case Negation => None
                case Not => None
                case Length => Some(NumberType)
              }
            case v: FunctionType => None
            case NilType => None
            case VoidType => None
            case VarargsType => None
            case v: TupleType => None
            case v: VectorType =>
              operation match {
                case Negation => None
                case Not => None
                case Length => Some(NumberType)
              }
          })
        if (unaryOperationLTypes == Set(None)) {
          Failure(None)
        } else {
          Success(unaryOperationLTypes.filterNot(_ == None).map(_.get))
        }
      }
      case Failure(_) => lTypes
    }
  }

  def processVariable(expressions: List[Expression], parentBlocks: List[List[Statement]]): TypeCheckResult = {
    if (expressions.size == 1 && expressions.head.isInstanceOf[IdentExpression]) {
      symbolTable.get(expressions.head.asInstanceOf[IdentExpression].ident.name, parentBlocks) match {
        case Some(ident) => ident.ltypes
        case None => NilType
      }
    } else if (expressions.size == 2 && expressions.head.isInstanceOf[IdentExpression] && expressions(1).isInstanceOf[TableIndexExpression]) {
      val name = expressions.head.asInstanceOf[IdentExpression].ident.name
      if (name != VarargsType.variableName) {
        symbolTable.get(name, parentBlocks) match {
          case Some(table) =>
            table.ltypes.head match {
              case TableType(ltypes) =>{
            	  val key = expressions(1).asInstanceOf[TableIndexExpression].index.asInstanceOf[StringExpression].string
            	  ltypes(StringType.create(key))
              }
              case VectorType(ltype, _) => ltype
              case _ => NilType
            }
          case _ => NilType
        }
      } else {
        symbolTable.get("...", parentBlocks).get.ltypes
      }
    } else {
      for (exp <- expressions) {
        val lTypes = processExpression(exp, parentBlocks)
        lTypes match {
          case Success(_) => {}
          case Failure(statement) => return lTypes
        }
      }
      expressions.filter(_.isInstanceOf)
      Success(allLTypes)
    }
  }

  def processIdent(ident: Ident, parentBlocks: List[List[Statement]]): TypeCheckResult = {
    symbolTable.get(ident.name, parentBlocks) match {
      case Some(symbol) => Success(symbol.ltypes)
      case None => NilType
    }
  }

  def processLambda(lambda: Lambda, parentBlocks: List[List[Statement]]): TypeCheckResult = {
    val lTypes = processFunctionBlock(lambda.body.block, List[List[Statement]]());
    lTypes match {
      case Success(lTypes) => FunctionType(lTypes)
      case Failure(statement) => lTypes
    }
  }

  def processTable(fields: List[(Field, Expression)], parentBlocks: List[List[Statement]]): TypeCheckResult = {
    val processResult = processFields(fields, parentBlocks)
    processResult match {
      case Success(_) => {
        fields.head._1 match {
          case UnnamedField => {
            val ltypes = (0 to fields.size).map(intToConst(_)).zip(fields.map(field => processExpression(field._2, parentBlocks).success.get.head))
            VectorType(ltypes.head._2, ltypes.size)
          }
          case NamedField(_) => {
            def fieldName(field: NamedField) = StringType.create(field.ident.name)
            val ltypes = new LinkedHashMap[L_Constant, LType]()
            fields.foreach(field => ltypes.put(fieldName(field._1.asInstanceOf[NamedField]), processExpression(field._2, parentBlocks).success.get.head))
            TableType(ltypes)
          }
          case ExpressionField(_) => throw new UnsupportedOperationException("Expression fields are not supported")
        }
      }
      case Failure(_) => processResult
    }
  }

  def processFields(fields: List[(Field, Expression)], parentBlocks: List[List[Statement]]): TypeCheckResult = {
    for (field <- fields) {
      val fieldKey = field._1
      if (fieldKey.isInstanceOf[ExpressionField]) {
        val lTypes = processExpression(fieldKey.asInstanceOf[ExpressionField].exp, parentBlocks)
        lTypes match {
          case Success(_) => {}
          case Failure(_) => return lTypes
        }
      }
      val lTypes = processExpression(field._2, parentBlocks)
      lTypes match {
        case Success(_) => {}
        case Failure(_) => return lTypes
      }
    }
    Success(Set[LType]())
  }

  def processFor(forStatement: For, parentBlocks: List[List[Statement]]): FailureStatement = {
    val lTypes = processExpression(forStatement.exp1, parentBlocks)
    lTypes match {
      case Success(ltypes) => symbolTable.put(new Symbol(forStatement.i.name, ltypes, Local(forStatement.block)))
      case Failure(None) => return forStatement
      case failure@Failure(_) => return Some(failure)
    }
    processExpression(forStatement.exp2, parentBlocks) match {
      case Success(_) => {}
      case Failure(None) => return forStatement
      case failure@Failure(_) => return Some(failure)
    }
    processExpression(forStatement.exp3, parentBlocks) match {
      case Success(_) => {}
      case Failure(None) => return forStatement
      case failure@Failure(_) => return Some(failure)
    }
    processBlock(forStatement.block, parentBlocks)
  }

  def processForEach(forStatement: Foreach, parentBlocks: List[List[Statement]]): FailureStatement = {
    var lTypes = Set[LType]()
    for (expression <- forStatement.exps) {
      processExpression(expression, parentBlocks) match {
        case Success(expLTypes) => lTypes ++= expLTypes
        case Failure(None) => return forStatement
        case failure@Failure(_) => return Some(failure)
      }
    }
    for (i <- forStatement.i) {
      symbolTable.put(new Symbol(i.name, lTypes, Local(forStatement.block)))
    }
    processBlock(forStatement.block, parentBlocks)
  }

  def processRepeat(repeat: Repeat, parentBlocks: List[List[Statement]]): FailureStatement = {
    processExpression(repeat.exp, parentBlocks) match {
      case Success(expLTypes) => {}
      case Failure(None) => return repeat
      case failure@Failure(_) => return Some(failure)
    }
    processBlock(repeat.block, parentBlocks)
  }

  def processWhile(whileStatement: While, parentBlocks: List[List[Statement]]): FailureStatement = {
    processExpression(whileStatement.exp, parentBlocks) match {
      case Success(expLTypes) => {}
      case Failure(None) => return whileStatement
      case failure@Failure(_) => return Some(failure)
    }
    processBlock(whileStatement.block, parentBlocks)
  }

  def processFunction(function: Function, parentBlocks: List[List[Statement]]): FailureStatement = {
    var i = 0;
    function.body.params.foreach { param =>
      val name = param match {
        case NamedParam(ident) => ident.name
        case VarArg => "..."
      }
      symbolTable.get(function.name, parentBlocks) match {
        case Some(functionSymbol) => {
          functionSymbol.ltypes.head match {
            case FunctionType(returnTypes, args) => symbolTable.put(new Symbol(name, Set(args(i).ltype), Local(function.body.block)))
            case _ => throw new IllegalStateException(function.name + " should be a function")
          }
          i = i + 1
        }
        case None => symbolTable.put(new Symbol(name, allLTypes, Local(function.body.block)))
      }
    }
    processFunctionBlock(function.body.block, List[List[Statement]]()) match {
      case Success(ltypes) => {
        val args = function.body.params.map(param => Argument(param match {
          case NamedParam(ident) => ident.name
          case VarArg => "..."
        }, NilType))
        symbolTable.put(new Symbol(function.name, Set[LType](FunctionType(ltypes, args)), Global))
        None
      }
      case Failure(Some(statement)) => statement
      case Failure(None) => throw new IllegalStateException()
    }
  }

  def processLocalFunction(function: LocalFunction, parentBlocks: List[List[Statement]]): FailureStatement = {
    processFunctionBlock(function.body.block, List[List[Statement]]()) match {
      case Success(ltypes) => {
        symbolTable.put(new Symbol(function.ident.name, Set[LType](FunctionType(ltypes)), Local(parentBlocks.last)))
        None
      }
      case Failure(Some(statement)) => statement
      case Failure(None) => throw new IllegalStateException()
    }
  }

  def processIf(ifStatement: If, parentBlocks: List[List[Statement]]): FailureStatement = {
    var lTypes = Set[LType]()
    for ((ifExp, block) <- ifStatement.ifExpBlocks) {
      processExpression(ifExp, parentBlocks) match {
        case Success(_) => {}
        case Failure(None) => return ifStatement
        case failure@Failure(_) => return Some(failure)
      }
      processBlock(block, parentBlocks)
    }
    processBlock(ifStatement.elseBlock, parentBlocks)
  }

  def processFunctionCall(functionCallExp: FunctionCallExpression, parentBlocks: List[List[Statement]]): TypeCheckResult = {
    val exps = functionCallExp.exps
    for (exp <- exps) {
      processExpression(exp, parentBlocks) match {
        case Success(_) => {}
        case Failure(None) => return Failure(None)
        case failure@Failure(_) => return failure
      }
    }
    val argsTypes = ListBuffer[LType]()
    for ((field, exp) <- functionCallExp.argsExp.args) {
      processExpression(exp, parentBlocks) match {
        case Success(ltypes) => argsTypes + ltypes.head
        case Failure(None) => return Failure(None)
        case failure@Failure(_) => return failure
      }
      if (field.isInstanceOf[ExpressionField]) {
        processExpression(field.asInstanceOf[ExpressionField].exp, parentBlocks) match {
          case Success(_) => {}
          case Failure(None) => return Failure(None)
          case failure@Failure(_) => return failure
        }
      }
    }
    if (exps.size == 1 && exps.head.isInstanceOf[IdentExpression] && symbolTable.get(exps.head.asInstanceOf[IdentExpression].ident.name, parentBlocks).isDefined) {
      val symbol = symbolTable.get(exps.head.asInstanceOf[IdentExpression].ident.name, parentBlocks).get
      symbol.ltypes.find(_.isInstanceOf[FunctionType]) match {
        case Some(functionType@FunctionType(returnLTypes, functionArgs)) => {
          val args = functionArgs.zip(argsTypes).map(arg => Argument(arg._1.name, arg._2))
          val function = new Symbol(symbol.name, Set(FunctionType(returnLTypes, args)), symbol.scope)
          symbolTable.put(function)
          Success(returnLTypes)
        }
        case _ => Failure(None)
      }
    } else {
      Success(allLTypes)
    }
  }

  def processLocalVarDeclarations(localVarDeclarations: LocalVarDeclarations, parentBlocks: List[List[Statement]]): FailureStatement = {
    for (variable <- localVarDeclarations.idents.zip(localVarDeclarations.exps)) {
      val lTypes = processExpression(variable._2, parentBlocks)
      lTypes match {
        case Success(lTypes) => symbolTable.put(new Symbol(variable._1.name, lTypes, Local(parentBlocks.last)))
        case Failure(None) => return localVarDeclarations
        case failure@Failure(_) => return Some(failure)
      }
    }
    None
  }

  def processVarsAssignment(varsAssignment: VarsAssignment, parentBlocks: List[List[Statement]]): FailureStatement = {
    varsAssignment.values.head match {
      case VarArgExpression => {
        for (variable <- varsAssignment.variables) {
          val varExpressions = variable.varExpression.exps
          if (varExpressions.size == 1) {
            varExpressions.head match {
              case IdentExpression(ident) => symbolTable.put(new Symbol(ident.name, allLTypes, Global))
              case _ => {}
            }
          }
        }
        None
      }
      case _ => {
        for ((variable, value) <- varsAssignment.variables.zip(varsAssignment.values)) {
          val lTypes = processExpression(value, parentBlocks)
          lTypes match {
            case Success(ltypes) if (ltypes == Set(VoidType)) => Some(varsAssignment)
            case Success(lTypes) =>
              val varExpressions = variable.varExpression.exps
              if (varExpressions.size == 1) {
                varExpressions.head match {
                  case IdentExpression(ident) => symbolTable.put(new Symbol(ident.name, lTypes, Global))
                  case _ => {}
                }
              } 
            case Failure(None) => return varsAssignment
            case failure@Failure(_) => return Some(failure)
          }
        }
        None
      }
    }
  }

  def processFunctionBlock(block: List[Statement], parentBlocks: List[List[Statement]]): TypeCheckResult = {
    processBlock(block, parentBlocks) match {
      case Some(failure) => return failure
      case None => {}
    }
    if (block.last.isInstanceOf[Return]) {
      val expressions = block.last.asInstanceOf[Return].expressions
      expressions.size match {
        case 0 => VoidType
        case 1 => processExpression(expressions.head, parentBlocks ::: List(block))
        case _ => {
          val lTypes = expressions.map(processExpression(_, parentBlocks ::: List(block)))
          if (lTypes.exists(_.isFailure)) {
            val failure = lTypes.filter(_.isFailure).head
            failure match {
              case Failure(Some(_)) => failure
              case Failure(None) => Failure(Some(block.last))
              case Success(_) => throw new IllegalStateException()
            }
          } else {
            TupleType(lTypes.map(_.success.get))
          }
        }
      }
    } else {
      val failure = processStatement(block.last, parentBlocks)
      failure match {
        case None => VoidType
        case Some(failure) => failure
      }
    }
  }

  def processBlock(block: List[Statement], parentBlocks: List[List[Statement]]): FailureStatement = {
    for (statement <- block) {
      val lType = processStatement(statement, parentBlocks ::: List(block))
      lType match {
        case None => {}
        case Some(failure) => return lType
      }
    }
    None
  }

  def processBinOperation(operation: BinOperation, expLeft: Expression, expRight: Expression, parentBlocks: List[List[Statement]]): TypeCheckResult = {
    val lTypesLeft = processExpression(expLeft, parentBlocks)
    if (lTypesLeft.isFailure) {
      return lTypesLeft
    }
    val lTypesRight = processExpression(expRight, parentBlocks)
    if (lTypesRight.isFailure) {
      return lTypesRight
    }
    getBinOperationType(operation, lTypesLeft.success.get, lTypesRight.success.get)
  }

  def getBinOperationType(operation: BinOperation, lTypesLeft: Set[LType], lTypesRight: Set[LType]): TypeCheckResult = {
    operation match {
      case ArithmeticOperation() =>
        if (lTypesLeft.contains(NumberType) && lTypesRight.contains(NumberType)) {
          NumberType
        } else {
          Failure(None)
        }
      case BooleanOperation() =>
        if (lTypesLeft.contains(BooleanType) && lTypesRight.contains(BooleanType)) {
          BooleanType
        } else {
          Failure(None)
        }
      case CompareOperation() =>
        if (lTypesLeft.contains(NumberType) && lTypesRight.contains(NumberType)) {
          BooleanType
        } else {
          Failure(None)
        }
      case Concat =>
        if (lTypesLeft.contains(StringType) && lTypesRight.contains(StringType)) {
          StringType
        } else {
          Failure(None)
        }
      case EqualityOperation() =>
        if (!(lTypesLeft & lTypesRight).isEmpty) {
          BooleanType
        } else {
          Failure(None)
        }
    }
  }

}