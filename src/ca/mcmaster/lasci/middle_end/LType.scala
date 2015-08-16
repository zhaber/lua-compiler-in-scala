package ca.mcmaster.lasci.middle_end
import ca.mcmaster.lasci.front_end.Statement
import scala.collection.mutable.LinkedHashMap
import org.slem.IRTree._

abstract sealed class LType {

  def defaultValue(): L_Constant = {
    this match {
      case NumberType => L_Double("0.0")
      case StringType => L_NullPointer(L_IntType(8))
      case BooleanType => L_Boolean(false)
      case TableType(ltypes) => L_Structure(ltypes.values.map(_.defaultValue()).toList)
      case VectorType(ltype, size) => L_Vector(List.fill(size)(ltype.defaultValue()))
      case FunctionType(returnTypes, args) => throw new IllegalStateException("Function allocaion")
      case TupleType(ltypes: List[Set[LType]]) => L_Vector(List())
      case VarargsType => throw new IllegalStateException("Varags allocaion")
      case NilType => L_NullPointer(L_VoidType())
      case VoidType => throw new IllegalStateException("Void type cannot be allocated")
    }

  }

  def toL_Type(): L_Type = this match {
    case NumberType => L_DoubleType()
    case StringType => L_PointerType(L_IntType(8))
    case BooleanType => L_IntType(1)
    case TableType(ltypes) => L_StructureType(ltypes.values.map(_.toL_Type()).toList)
    case FunctionType(returnTypes, args) => L_PointerType(L_FunctionType(returnTypes.head.toL_Type, args.map(_.ltype.toL_Type())))
    case TupleType(ltypes: List[Set[LType]]) => L_VectorType(ltypes.size, ltypes.head.head.toL_Type())
    case VarargsType => L_VarArgsType()
    case NilType => L_PointerType(L_OpaqueType())
    case VoidType => L_VoidType()
    case VectorType(ltype, size) => L_VectorType(size, ltype.toL_Type())
  }

}

abstract sealed class ValueLType extends LType {
  type Type
  def create(n: Type): L_Value
}

case object NumberType extends ValueLType {
  type Type = Double
  override def create(n: Type) = L_Double("" + n)
}

case object StringType extends ValueLType {
  type Type = String

  override def create(s: Type) = L_String(s + "\\00")
}

case object BooleanType extends ValueLType {
  type Type = Boolean
  override def create(b: Type) = L_Boolean(b)
}

case class TableType(ltypes: LinkedHashMap[L_Constant, LType]) extends ValueLType {
  type Type = Map[L_Value, L_Value]
  override def create(table: Type) = L_Structure(table.values.toList)
}

case class VectorType(ltype: LType, size : Int) extends ValueLType {
  type Type = List[L_Value]
  override def create(vector: Type) = L_Vector(vector)
}

class FunctionDefinition(val name: String, val args: List[L_Argument], val blocks: List[L_Block])

case class Argument(name: String, ltype: LType)

case class FunctionType(returnLTypes: Set[LType], args: List[Argument] = List()) extends ValueLType {
  type Type = FunctionDefinition
  override def create(definition: Type): L_FunctionDefinition = {
    L_FunctionDefinition(returnLTypes.head.toL_Type, definition.blocks, definition.name, definition.args)
  }
}

case class TupleType(ltypes: List[Set[LType]]) extends ValueLType {
  type Type = List[L_Value]
  override def create(variables: Type): L_Value = L_Vector(variables)
}

case object VarargsType extends LType {
	val variableName = "arg"
    val argName = "..."
}

case object NilType extends LType {
  def create() = L_NullPointer(L_OpaqueType())
}

case object VoidType extends LType
