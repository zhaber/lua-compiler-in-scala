/**
 * This file is part of Slem.
 *
 * Copyright 2011 Timothy Morton.
 *
 * Slem is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Slem is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Slem.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */


package org.slem.tests
 
import org.scalatest.Spec
import org.slem.IRTree._
import org.slem.IRTreeEncoder
import org.kiama.util.Console
import org.kiama.util.StringEmitter
 
class FunctionSpec extends Spec {

    private def emitTest(instr : L_FunctionDeclaration) : String = 
    {
        val e = new StringEmitter()
		val encoder = new IRTreeEncoder(e)
		encoder.encodeFunctionDeclaration(instr)
		e.result
    }
    private def emitFuncDefTest(instr : L_FunctionDefinition) : String = 
    {
        val e = new StringEmitter()
		val encoder = new IRTreeEncoder(e)
		encoder.encodeFunctionDefinition(instr)
		e.result
    }
	private def typeTest(instr : L_Instruction) : String =
	{
	    val e = new StringEmitter()
		val encoder = new IRTreeEncoder(e)
		encoder.encodeType(instr->resultType)
		e.result
	}	
	private def typeTestBasic(typ : L_Type) : String =
	{
	    val e = new StringEmitter()
		val encoder = new IRTreeEncoder(e)
		encoder.encodeType(typ)
		e.result
	}	    
    describe("Function Declarations: ") {
      
      it("full fn test") {
        expect("declare linktest vistest cctest retattrs1 retattrs2 i64 @myFunc(i32, i64) align 5 gc " + '"' + "gctest" + '"' + " \n")
        {
            val myfunc = L_FunctionDeclaration(
                L_IntType(64),
                funcName = "myFunc",
                arguments = List(L_IntType(32), L_IntType(64)),
                linkage = "linktest",
                visibilityStyle = "vistest",
                callConvention = "cctest",
                returnAttributes = List("retattrs1", "retattrs2"),
                alignment = 5,  
                garbageCollector = "gctest"
            )
            emitTest(myfunc)
        }
      }
    }
    
    describe("Function Definitions: ") {
    
      it("full fn test") {
        expect("define linktest vistest cctest retattrs1 retattrs2 i64 @myFunc(i32 %param0, i64 %param1) fnattr1 fnattr2 section " + '"' + "sectiontest" + '"' + " align 5 gc " + '"' + "gctest" + '"' + " {\nblock0:\n  %0 = add i32 1, 2\n  ret i32 0\n\n}\n\n")
        {
            val myBlock = L_Block(List(L_Add(1,2)), L_Ret(0))
            val myfunc = L_FunctionDefinition(
                L_IntType(64),
                List(myBlock),
                funcName = "myFunc",
                arguments = List(L_IntType(32), L_IntType(64)),
                linkage = "linktest",
                visibilityStyle = "vistest",
                callConvention = "cctest",
                returnAttributes = List("retattrs1", "retattrs2"),
                funcAttributes = List("fnattr1", "fnattr2"),
                section = "sectiontest",
                alignment = 5,  
                garbageCollector = "gctest"
            )
            emitFuncDefTest(myfunc)        
        }
      }
      
      it("full fn test - with named arguments") {
        expect("define linktest vistest cctest retattrs1 retattrs2 i64 @myFunc(i32 %moo1, i64 %moo2) fnattr1 fnattr2 section " + '"' + "sectiontest" + '"' + " align 5 gc " + '"' + "gctest" + '"' + " {\nblock0:\n  %0 = add i32 1, 2\n  ret i32 0\n\n}\n\n")
        {
            val myBlock = L_Block(List(L_Add(1,2)), L_Ret(0))
            val myfunc = L_FunctionDefinition(
                L_IntType(64),
                List(myBlock),
                funcName = "myFunc",
                arguments = List(L_Argument(L_IntType(32), argName = "moo1"), L_Argument(L_IntType(64), argName = "moo2")),
                linkage = "linktest",
                visibilityStyle = "vistest",
                callConvention = "cctest",
                returnAttributes = List("retattrs1", "retattrs2"),
                funcAttributes = List("fnattr1", "fnattr2"),
                section = "sectiontest",
                alignment = 5,  
                garbageCollector = "gctest"
            )
            emitFuncDefTest(myfunc)        
        }
      }
      it("full fn test - with named arguments - Type test") {
        expect("i64( i32, i64 )*")
        {
            val myBlock = L_Block(List(L_Add(1,2)), L_Ret(0))
            val myfunc = L_FunctionDefinition(
                L_IntType(64),
                List(myBlock),
                funcName = "myFunc",
                arguments = List(L_Argument(L_IntType(32), argName = "moo1"), L_Argument(L_IntType(64), argName = "moo2")),
                linkage = "linktest",
                visibilityStyle = "vistest",
                callConvention = "cctest",
                returnAttributes = List("retattrs1", "retattrs2"),
                funcAttributes = List("fnattr1", "fnattr2"),
                section = "sectiontest",
                alignment = 5,  
                garbageCollector = "gctest"
            )
            typeTestBasic(myfunc->resultType)        
        }
      }
      
    }
    
    
    
}