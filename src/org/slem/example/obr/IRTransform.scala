/*
    Copyright 2010 Timothy Morton
    
    This file is part of Slem.

    Slem is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Slem is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Slem.  If not, see <http://www.gnu.org/licenses/>.
*/

package org.slem.example.obr

object IRTransform {

    import org.slem.example.obr.ObrTree._
    import org.slem.example.obr.SemanticAnalysis._
    import org.slem.IRTree._
    import org.slem.util.IRTreeFactory._
    import org.slem.example.obr.SymbolTable._
    import org.kiama.attribution.Attribution._
    import scala.collection.mutable.HashMap
    
    var valuemap = HashMap[String, L_Value] ()
    var arraySizeMap = HashMap[String, Int] ()
    
    var blockList : List[L_Block] = List()
    var currentBlockInstructions : List[L_Instruction] = List()
    var currentBlockLabel = L_Label("entry")
    
    val argc = L_Argument(L_IntType(32), argName = "argc")
    val argv = L_Argument(L_PointerType(L_PointerType(L_IntType(8))), argName = "argv")
    
    var nextParamIndex = 0
    
    def getNextParamIndex() : Int =
    {
        nextParamIndex = nextParamIndex + 1
        nextParamIndex
    }
    
    val code : ObrInt ==> L_Program = 
    {
        case ObrInt (_, decls, stmts, _) =>
        {
            resetMacros()
            val errorString = L_GlobalVariable(L_String("Error: Incorrect number of arguments\\00"), isConstant = true)
            valuemap = HashMap[String, L_Value] ()
            nextParamIndex = 0
            val numParams = decls.filter(d=>
                d match
                {
                    case p : IntParam => true
                    case _ => false
                }
            ).size
            
            //Check argument count to match number of inputs.
            blockList = List()
            currentBlockLabel = L_Label("entry")
            currentBlockInstructions = List()
            val comp = L_ICmpSLT(argc, numParams + 1)
            addInstruction(comp)
            val obrEntryLabel = L_Label("obrentry")
            val argumentFailureExitLabel = L_Label("argfailure")
            val testbr = L_BrCond(comp, argumentFailureExitLabel, obrEntryLabel)
            addInstruction(testbr)
            
            //build argument count failure routine
            currentBlockLabel = argumentFailureExitLabel
            val errorPrintMacro = L_Macro_Puts(errorString)
            addInstructions(errorPrintMacro ::: List(L_Ret(0)))
            
            //build main program
            currentBlockLabel = obrEntryLabel
            codeProgram(decls,stmts)
            val obrArgs = List(argc, argv)
            val obrFunc = L_FunctionDefinition(L_IntType(32), blockList, funcName = "main", arguments = obrArgs)
            val mainmodule = L_Module(List(obrFunc,errorString) ::: imports)
            L_Program(List(mainmodule))
        }
    }
    
    private def codeProgram(decls : List[Declaration], stmts : List[Statement]) : List[L_Block] =
    {

        for(d<-decls)
        {
            addInstructions(codeDeclaration(d))
        }
        for(s<-stmts)
        {
            codeStatement(s)
        }
        addInstruction(L_Ret(0))
        blockList
    }
    
    private def addInstructions(instrs : List[L_Instruction])
    {
        for(instr <- instrs)
        {
            addInstruction(instr)
        }
    }
    
    private def addInstruction(instr : L_Instruction)
    {
        instr match
        {
            case i : L_TerminatorInstruction =>
            {
                /*
                if(currentBlockInstructions.size == 0)
                {
                    currentBlockInstructions = currentBlockInstructions ::: List(L_Add(0,0)) //add junk instruction - llvm is quirky :P
                }
                */
                blockList = blockList ::: List(L_Block(currentBlockInstructions, i, label = currentBlockLabel))
                currentBlockInstructions = List()
                currentBlockLabel = L_Label("")
            }
            case _ =>
            {
                currentBlockInstructions = currentBlockInstructions ::: List(instr)
            }
        }	    
    }
    
    private def codeDeclaration(decl : Declaration) : List[L_Instruction] =
    {
        decl match
        {
            //Handle an int variable declaration
            case IntVar(idn) =>
            {
                val allocation = L_Alloca(L_IntType(32))
                valuemap(idn) = allocation
                List(allocation)
            }
            
            //Feed the value of the intparam into a new allocation
            case IntParam(idn) =>
            {
                val getParamValue = L_Macro_GetIntArgument(argv, getNextParamIndex())
                val allocation = L_Alloca(L_IntType(32))
                val pvalue = getParamValue.last
                pvalue match
                {
                    case n : L_Value =>
                    {
                        val storage = L_Store(n, allocation)
                        valuemap(idn) = allocation
                        getParamValue ::: List(allocation, storage)                    
                    }
                    case _ => List()
                }

            }
            
            //Handle a boolean variable declaration
            case BoolVar(idn) =>           
            {
                val allocation = L_Alloca(L_IntType(32))
                valuemap(idn) = allocation
                List(allocation)
            }
            case ArrayVar(idn, size) =>
            {
                val allocation = L_Alloca(L_ArrayType(size, L_IntType(32)))
                valuemap(idn) = allocation
                arraySizeMap(idn) = size		//needed??
                List(allocation)
            }
            case IntConst(idn, value) =>
            {
                val allocation = L_Alloca(L_IntType(32))
                valuemap(idn) = allocation
                val storage = L_Store(value, allocation)
                List(allocation, storage)
            }
            case _ => List()
        }
    }
    
    private def codeStatement(stmt : Statement) : Int = 
    {
        stmt match
        {
            case AssignStmt(assnode, r) => 			
            {
                assnode match
                {
                    case IdnExp(idn) =>
                    {
                        val rvalue = codeExpression(r)
                        if(valuemap.contains(idn))
                        {
                            var ident = valuemap(idn)
                            addInstruction(L_Store(rvalue, ident))
                        }					
                    }
                    //case FieldExp(idn) => //Fields not defined properly in Obr - are they ints or bools?
                    case IndexExp(idn, indx) =>
                    {
                        val rvalue = codeExpression(r)
                        val indexexp = codeExpression(indx)

                        //var ptr = f.getNewSSA
                        val ptr = L_GetElementPtr(L_PointerType(L_ArrayType(arraySizeMap(idn), L_IntType(32))), valuemap(idn), List(0, indexexp))
                        val storeInstr = L_Store(rvalue, ptr)
                        addInstructions(List(ptr, storeInstr))
                        //f.currentBlock.addInstruction(IRSimpleInstruction("%" + ptr.identNum + " = getelementptr inbounds [" + arraySizeMap(idn) + " x i32]* %" + ssamap(idn) + ", i32 0, i32 %" + indexexp.identNum))
                        //f.currentBlock.addInstruction(IRInstruction_store(false, IRIntegerType(32), rvalue, ptr, null, 0))                  
                                            
                    }
                }
            }
            case e : ExitStmt =>
            {
                var p = e.parent
                var enclosingloopfound = false
                while(!enclosingloopfound)
                {
                    p match
                    {
                        case l : LoopStmt => enclosingloopfound = true		//TODO - insert check for infinite loop
                                             val exitlbl = l->getExitBlock
                                             addInstruction(L_Br(exitlbl))
                        case _ => p = p.parent
                    }
                }			
            }
            case ForStmt(idn, min, max, body) =>
            {
            
                val indval = codeExpression(min)
                val ptr = valuemap(idn)
                val storeInstr = L_Store(indval, ptr)
                addInstruction(storeInstr)
                
                //Define the block names:
                //forblock  - evaluates the control statement before each loop
                //stmtblock - contains the statements in the loop body
                //exitblock - new block where execution continues after completing the for loop
                var forblock = L_Label("")
                var stmtblock = L_Label("")
                var exitblock = L_Label("")
                
                
                //Now that we've defined the min expression, jump to the for loop evaluation block
                addInstruction(L_Br(forblock))
                
                
                //Create the for evaluation block
                currentBlockLabel = forblock
                
                
                //Evaluate the max expression so we can compare against it and store it in a new SSA
                val maxvalue = codeExpression(max)             
                
                //Load the current value of the value pointed to by the identifier from memory into
                //a new SSA called currentindexval
                var ptr1 = valuemap(idn)
                
                val currentIndexVal = L_Load(L_PointerType(L_IntType(32)), ptr1)
                addInstruction(currentIndexVal)
                
                
                //Compare the current index value with the maximum index value - signed less then or equal
                //Store the result in condbool
                
                val condbool = L_ICmpSLE(currentIndexVal, maxvalue)
                addInstruction(condbool)
                
                //If the result is true, jump to the block containing the for loop body,
                //else jump to the exit block
                addInstruction(L_BrCond(condbool, stmtblock, exitblock))
                
                //Create the block containing the statements in the for loop body
                currentBlockLabel = stmtblock
                for(s <- body)
                {
                    codeStatement(s)
                }
                
                
                //Increment the loop index
                var ptr2 = valuemap(idn)
                val oldIndexVal = L_Load(L_PointerType(L_IntType(32)), ptr2)
                val newIndexVal = L_Add(oldIndexVal, 1)
                val storeNewIndex = L_Store(newIndexVal, ptr2)
                addInstructions(List(oldIndexVal, newIndexVal, storeNewIndex))
                
                //Terminate the block containing the for loop body with a branch back
                //to the for loop evaluation block
                addInstruction(L_Br(forblock))
                
                //Add the exit block within which statements after the for loop will reside.
                currentBlockLabel = exitblock  			
            }
            case IfStmt(cond, thens, elses) =>
            {
                //Define the block labels:
                //thenblock - contains the list of statements carried out when the condition holds true
                //elseblock - contains the list of statements carried out when the condition is false
                //exitblock - contains the statements after the IF statement executed after resolving
                //            the IF statement
                val thenBlock = L_Label("")
                val elseBlock = L_Label("")
                val exitBlock = L_Label("")
                
                //Evaluate the condition expression and store it in a new SSA
                val expValue = codeExpression(cond)
                
                //Resolve the condition expression into a boolean (1 bit integer) so we can use it in a conditional br
                val condBool = L_ICmpNE(expValue, 0)
                
                //If the condition boolean holds true, branch to the THEN block, otherwise, branch to the ELSE block
                val condBranch = L_BrCond(condBool, thenBlock, elseBlock)
                addInstructions(List(condBool, condBranch))
                
                currentBlockLabel = thenBlock
                for(s<-thens)
                {
                    codeStatement(s)
                }				
                
                val exitBranch = L_Br(exitBlock)
                addInstruction(exitBranch)
                
                currentBlockLabel = elseBlock
                for(s<-elses)
                {
                    codeStatement(s)
                }				
                
                val exitBranch2 = L_Br(exitBlock)
                addInstruction(exitBranch2)
                
                currentBlockLabel = exitBlock
                
            }
            case l : LoopStmt =>
            {
                val body = l.body
                val loopblock = L_Label("")
                val loopentry = L_Br(loopblock)
                addInstruction(loopentry)
                currentBlockLabel = loopblock
                for(s<-body)
                {
                    codeStatement(s)
                }
                val loopback = L_Br(loopblock)
                addInstruction(loopback)
                currentBlockLabel = l->getExitBlock
            }
            case ReturnStmt(expr) =>
            {
                val expValue = codeExpression(expr)
                val returnPrintStmt = L_Macro_PrintLine(expValue)
                val ret = L_Ret(expValue)
                
                addInstructions(returnPrintStmt ::: List(ret))
            }
            case WhileStmt(cond, body) =>
            {
                val whileblock = L_Label("")
                val stmtblock = L_Label("")
                val exitblock = L_Label("")

                addInstruction(L_Br(whileblock))
                
                currentBlockLabel = whileblock
                val expvalue = codeExpression(cond)
                val condbool = L_ICmpNE(expvalue, 0)
                val condbr = L_BrCond(condbool, stmtblock, exitblock)
                
                addInstructions(List(condbool, condbr))
                
                currentBlockLabel = stmtblock
                for(s <- body)
                {
                    codeStatement(s)
                }
                
                addInstruction(L_Br(whileblock))
                
                currentBlockLabel = exitblock	    
            }
        }
        0
    }
    
    
    val getExitBlock : ObrNode ==> L_Label =
    {
        attr 
        {
            //case l : LoopStmt => L_Label("")
            case _ => L_Label("")
        }
    }
    
    
    //Adds the instructions required to encode this expression to the current block
    private def codeExpression(exp : Expression) : L_Value = 
    {
        exp match
        {
            case AndExp(l,r)         =>				
            {
                val LHS = codeExpression(l)
                val RHS = codeExpression(r)
                val leftBool = L_ICmpSGT(LHS, 0)
                val rightBool = L_ICmpSGT(RHS, 0)
                val andBool = L_And(leftBool, rightBool)
                val zexted = L_ZExt(andBool, L_IntType(32))
                addInstructions(List(leftBool, rightBool, andBool, zexted))
                zexted
            }
            case BoolExp(value)      => 			
            {
                var intval = 0
                if(value)
                {
                    intval = 1
                }
                val boolval = L_Add(intval, 0)
                addInstruction(boolval)
                boolval
            }
            case EqualExp(l,r)       =>				
            {
                val LHS = codeExpression(l)
                val RHS = codeExpression(r)
                val isEqual = L_ICmpEQ(LHS, RHS)
                val zexted = L_ZExt(isEqual, L_IntType(32))
                addInstructions(List(isEqual, zexted))
                zexted
            }
            case FieldExp(idn,field) => null 
            case GreaterExp(l,r)     =>				
            {
                val LHS = codeExpression(l)
                val RHS = codeExpression(r)
                val isEqual = L_ICmpSGT(LHS, RHS)
                val zexted = L_ZExt(isEqual, L_IntType(32))
                addInstructions(List(isEqual, zexted))
                zexted			
            }
            case IdnExp(idn)         =>				
            {
                val loading = L_Load(L_PointerType(L_IntType(32)), valuemap(idn))
                addInstruction(loading)
                loading
            }
            case IndexExp(idn, indx) =>				
            {
                val indxval = codeExpression(indx)
                //val typeidx = L_TypeIndex(L_IntType(32), indxval)
                val elementPtr = L_GetElementPtr(valuemap(idn)->resultType, valuemap(idn), List(0, indxval), inBounds = true)
                val elementVal = L_Load(L_PointerType(L_IntType(32)), elementPtr)
                addInstructions(List(elementPtr, elementVal))
                elementVal
            }
            case IntExp(num)         =>				
            {
                val intVal = L_Add(num, 0)
                addInstruction(intVal)
                intVal
            }
            case LessExp(l,r)        =>				
            {
                val LHS = codeExpression(l)
                val RHS = codeExpression(r)
                val isEqual = L_ICmpSLT(LHS, RHS)
                val zexted = L_ZExt(isEqual, L_IntType(32))
                addInstructions(List(isEqual, zexted))
                zexted					
            }
            case MinusExp(l,r)       =>				
            {
                val LHS = codeExpression(l)
                val RHS = codeExpression(r)
                val out = L_Sub(LHS, RHS)
                addInstruction(out)
                out
            }
            case ModExp(l,r)         =>				
            {
                val LHS = codeExpression(l)
                val RHS = codeExpression(r)
                val out = L_URem(LHS,RHS)
                addInstruction(out)
                out
            }
            case NegExp(e)           =>				
            {
                val expval = codeExpression(e)
                val out = L_Sub(0, expval)
                addInstruction(out)
                out
            }
            case NotEqualExp(l,r)    =>
            {
                val LHS = codeExpression(l)
                val RHS = codeExpression(r)
                val isEqual = L_ICmpNE(LHS, RHS)
                val zexted = L_ZExt(isEqual, L_IntType(32))
                addInstructions(List(isEqual, zexted))
                zexted				    
            }
            case NotExp(e)           =>				
            {
                val expval = codeExpression(e)
                val isEqual = L_ICmpNE(expval, 0)
                val zexted = L_ZExt(isEqual, L_IntType(32))
                addInstructions(List(isEqual, zexted))
                zexted					
            }
            case OrExp(l,r)          =>				
            {
                val LHS = codeExpression(l)
                val RHS = codeExpression(r)
                val leftBool = L_ICmpSGT(LHS, 0)
                val rightBool = L_ICmpSGT(RHS, 0)
                val orVal = L_Or(leftBool, rightBool)
                val zexted = L_ZExt(orVal, L_IntType(32))
                addInstructions(List(leftBool, rightBool, orVal, zexted))
                zexted
            }
            case PlusExp(l,r)        =>				
            {
                val LHS = codeExpression(l)
                val RHS = codeExpression(r)
                val out = L_Add(LHS, RHS)
                addInstruction(out)
                out
            }
            case SlashExp(l,r)       =>
            {
                val LHS = codeExpression(l)
                val RHS = codeExpression(r)
                val out = L_SDiv(LHS, RHS)
                addInstruction(out)
                out
            }
            case StarExp(l,r)        => 
            {
                val LHS = codeExpression(l)
                val RHS = codeExpression(r)
                val out = L_Mul(LHS, RHS)
                addInstruction(out)
                out
            }
            case _                   => null 
        }
    }
}
    