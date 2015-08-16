package ca.mcmaster.lasci.back_end

import org.slem.IRTree._
import ca.mcmaster.lasci.middle_end._
import ca.mcmaster.lasci.middle_end.Global
import ca.mcmaster.lasci.middle_end.Global

object Allocator {

  def allocateGlobal(symbol: Symbol): L_GlobalVariable = {
	val ltype = symbol.ltypes.head
    val allocation = L_GlobalVariable(ltype.defaultValue())
    symbol.value = allocation
    allocation
  }
  
  def allocateLocal(symbol: Symbol): L_Alloca = {
	val ltype = symbol.ltypes.head
    val allocation = L_Alloca(ltype.toL_Type())
    symbol.value = allocation
    allocation
  }
  
  def allocateString(size: Long): L_Alloca = {
    val allocation = L_Alloca(L_ArrayType(size, L_IntType(8)))
    allocation
  }

}