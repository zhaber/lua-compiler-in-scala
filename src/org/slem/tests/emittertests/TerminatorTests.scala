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
 
class TerminatorSpec extends Spec {

    def emitTest(instr : L_TerminatorInstruction) : String = 
    {
        val e = new StringEmitter()
		val encoder = new IRTreeEncoder(e)
		encoder.encodeTerminator(instr)
		e.result
    }
	
    describe("Terminator - Ret: ") {
 
      it("L_Ret - Basic") {
        expect("ret i32 1") 
	    {  
	      emitTest(L_Ret(1))
	    }
      }
      it("L_Ret - Type Inference") {
        expect("ret i64 1") 
	    { 
	      emitTest(L_Ret(1 : Long))
	    }
      }
      it("L_Ret - Void") {
        expect("ret void")
        {
          emitTest(L_Ret(L_Void()))
        }
      }
    }
    
    describe("Terminator - Br: ") {
      it("L_Br - Unconditional") 
      {
        expect("br label %mylabel") 
	    {  
          var myLabel = L_Label("mylabel")
	      emitTest(L_Br(myLabel))
	    }
      }   
      it("L_BrCond - Conditional") 
      {
        expect("br i1 0, label %mylabel, label %mylabel2") 
	    {  
          var myLabel = L_Label("mylabel")
          var myLabel2 = L_Label("mylabel2")
	      emitTest(L_BrCond(false, myLabel, myLabel2))
	    }
      }       
    }

    describe("Terminator - Switch: ") {
      it("L_Switch - Uncond Br")
      {
        expect("switch i32 0, label %mylabel [ ]") 
	    {  
          var myLabel = L_Label("mylabel")
	      emitTest(L_Switch(0, myLabel, List()))
	    }      
      }
      it("L_Switch - One Destination")
      {
        expect("switch i32 0, label %mylabel [ i32 1, label %mylabel2 ]") 
	    {  
          var myLabel = L_Label("mylabel")
          var myLabel2 = L_Label("mylabel2")
          var valuelab = L_ValueLabel(1, myLabel2)
	      emitTest(L_Switch(0, myLabel, List(valuelab)))
	    }      
      }
      it("L_Switch - Multiple Destinations")
      {
        expect("switch i32 0, label %mylabel [ i32 1, label %mylabel2 i32 2, label %mylabel3 i32 3, label %mylabel4 ]") 
	    {  
          var myLabel = L_Label("mylabel")
          var myLabel2 = L_Label("mylabel2")
          var myLabel3 = L_Label("mylabel3")
          var myLabel4 = L_Label("mylabel4")
          var valuelab = L_ValueLabel(1, myLabel2)
          var valuelab2 = L_ValueLabel(2, myLabel3)
          var valuelab3 = L_ValueLabel(3, myLabel4)
          
	      emitTest(L_Switch(0, myLabel, List(valuelab, valuelab2, valuelab3)))
	    }      
      }    
    }
    
    describe("Terminator - IndirectBr: ") {
      it("L_IndirectBr - Indirect Branch")
      {
        expect("indirectbr i8* null, [ label %mylabel, label %mylabel2 ]") 
	    {
          var myval = L_NullPointer(L_IntType(8))
          var myLabel = L_Label("mylabel")
          var myLabel2 = L_Label("mylabel2")
	      emitTest(L_IndirectBr(myval, List(myLabel, myLabel2)))
	    }      
      }    
    }
    
    describe("Terminator - Invoke: ") {
      it("L_Invoke - Simple")
      {
        expect("%0 = invoke i32( i32 )* @myfunc( i32 1 ) to label %moo unwind label %blah")
        {
          val myfunc = L_FunctionDeclaration(L_IntType(32), funcName = "myfunc", arguments = List(L_IntType(32)))
          emitTest(L_Invoke(myfunc, List(L_Int(32, 1)), "moo", "blah"))
        }
      }
      
      it("L_Invoke - Fn Ptr")
      {
        expect("%0 = invoke i8( i32 )* %1( i32 1 ) to label %moo unwind label %blah")
        {
          val myfunc = L_FunctionDeclaration(L_IntType(8), funcName = "myfunc", arguments = List(L_IntType(32)))
          val myfuncPtrAlloca = L_Alloca(L_PointerType(myfunc->resultType))
          val myfuncPtrStor = L_Store(myfunc, myfuncPtrAlloca)
          val myfuncPtrLoad = L_Load(L_PointerType(myfunc->resultType), myfuncPtrAlloca)
          emitTest(L_Invoke(myfuncPtrLoad, List(L_Int(32, 1)), "moo", "blah"))
        }
      }
      
      it("L_Invoke - All invoke variables")
      {
        expect("%0 = invoke cconvtest retattrstest retattrstest2 i32( i32 )* @myfunc( i32 1 ) attrstest attrstest2 to label %moo unwind label %blah")
        {
          val myfunc = L_FunctionDeclaration(L_IntType(32), funcName = "myfunc", arguments = List(L_IntType(32)))
          emitTest(L_Invoke(myfunc, List(L_Int(32, 1)), "moo", "blah", callConv = "cconvtest", retAttrs = List("retattrstest", "retattrstest2"), attrs = List("attrstest", "attrstest2")))
        }      
      }
      
    }
    
    describe("Terminator - Unwind: ") {
      it("L_Unwind")
      {
        expect("unwind")
        {
          emitTest(L_Unwind())
        }
      }
    }
    
    describe("Terminator - Unreachable: ") {
      it("L_Unreachable")
      {
        expect("unreachable")
        {
          emitTest(L_Unreachable())
        }
      }
    }
    
    
    
}