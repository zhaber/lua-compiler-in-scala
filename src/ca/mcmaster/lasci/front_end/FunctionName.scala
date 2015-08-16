package ca.mcmaster.lasci.front_end

case class FunctionName(idents: List[Ident]) 

case class MethodName(objectName: List[Ident], methodName : Ident) extends FunctionName(objectName)