package ca.mcmaster.lasci.middle_end
import ca.mcmaster.lasci.front_end.Statement
import org.slem.IRTree._

abstract sealed class Scope {
	override def toString() = this match {
		case Global => "Global"
		case Local(scope) => scope.hashCode.toString
	}
}

case object Global extends Scope

case class Local(declarationBlock: List[Statement]) extends Scope

class Symbol(val name: String, val ltypes: Set[LType], val scope: Scope, var value: L_Value = L_NullPointer(L_OpaqueType()) ) {
	
	override def equals(that: Any) = { 
		if (that.isInstanceOf[Symbol]) {
			that.asInstanceOf[Symbol].name == name && that.asInstanceOf[Symbol].scope == scope 
		} else {
			false
		}
	}
	
	override def hashCode = name.hashCode + scope.hashCode

	override def toString() = {
	  name + ":\t" + ("" /:  ltypes)(_.toString + "," + _.toString).tail + "(scopeID: " + scope + ")"
    }
	
}

