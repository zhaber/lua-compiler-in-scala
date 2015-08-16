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
 
class ConstantSpec extends Spec {

    def emitTest(instr : L_Instruction) : String = 
    {
        val e = new StringEmitter()
		val encoder = new IRTreeEncoder(e)
		encoder.encodeInstruction(instr)
		e.result
    }
    
    def vtest(value : L_Value) : String =
    {
        val e = new StringEmitter()
        val encoder = new IRTreeEncoder(e)
        encoder.encodeValue(value)
        e.result
    }
    
    def mtest(met : L_BaseMetadata) : String =
    {
        val e = new StringEmitter()
        val encoder = new IRTreeEncoder(e)
        encoder.encodeMetadata(met)
        e.result
    }
    
    def ptest(tree : L_Program) : String =
    {
        val e = new StringEmitter()
        val encoder = new IRTreeEncoder(e)
        encoder.encodeTree(tree)
        e.result
    }
    
    describe("Constant - blockaddress ") {
      it("L_Blockaddress")
      {
        expect("blockaddress(@main, %block0)")
        {
          val mainfunc = L_FunctionDefinition(L_IntType(32), List(), funcName = "main")
          val blabel = L_Label("%block0")
          //val block = L_Block(List(), L_Ret(0))
          val badd = L_BlockAddress(mainfunc, blabel)
          vtest(badd)
        }
      }
    }
    
    describe("Simple Constants") {
      it("L_Boolean")
      {
        expect("%0 = add i1 true, false")
        {
          val myval1 = L_Boolean(true)
          val myval2 = L_Boolean(false)
          val addinstr = L_Add(myval1, myval2)
          emitTest(addinstr)
        }
      }
      it("L_Boolean implicit conversion")
      {
        expect("%0 = add i1 1, 0")
        {
          val addinstr = L_Add(true, false)
          emitTest(addinstr)
        }
      }
      it("L_Int")
      {
        expect("%0 = add i33 16, 0")
        {
          val myval1 = L_Int(33, 16)
          val myval2 = L_Int(33, 0)
          val addinstr = L_Add(myval1, myval2)
          emitTest(addinstr)
        }
        expect("%0 = add i32 15, 32")
        {
           emitTest(L_Add(15,32))
        }
        expect("%0 = add i64 20, 25")
        {
          val long1 : Long = 20
          val long2 : Long = 25
          emitTest(L_Add(long1, long2))
        }
      }
      it("L_Float")
      {
        expect("%0 = fadd float 1.0, 2.0")
        {
          val f1 : Float = 1
          val f2 : Float = 2
          emitTest(L_FAdd(f1, f2))
        }
        expect("%0 = fadd float 1.0, 2.0")
        {
          emitTest(L_FAdd(L_Float("1.0"), L_Float("2.0")))
        }
      }
      it("L_Double")
      {
        expect("%0 = fadd double 1.0, 2.0")
        {
          val double1 : Double = 1.0
          val double2 : Double = 2.0
          emitTest(L_FAdd(double1, double2))
        }
        expect("%0 = fadd double 1.0, 2.0")
        {
          emitTest(L_FAdd(L_Double("1.0"), L_Double("2.0"))) 
        }
      }
      it("L_FP128")
      {
        expect("%0 = fadd fp128 1.0, 2.0")
        {
          emitTest(L_FAdd(L_FP128("1.0"), L_FP128("2.0")))
        }
      }
      it("L_X86FP80")
      {
        expect("%0 = fadd x86fp80 1.0, 2.0")
        {
          emitTest(L_FAdd(L_X86FP80("1.0"), L_X86FP80("2.0")))
        }
      }
      it("L_PPCFP128")
      {
        expect("%0 = fadd ppcfp128 1.0, 2.0")
        {
          emitTest(L_FAdd(L_PPCFP128("1.0"), L_PPCFP128("2.0")))
        }
      }
      it("L_String")
      {
        expect("%0 = add [12 x i8] c" + '"' + "String Test\\00" + '"' + ", 0")
        {
          emitTest(L_Add(L_String("String Test\\00"), 0))
        }
      }     
      it("L_ZeroInitialiser")
      {
        expect("%0 = add i8 zeroinitializer, zeroinitializer")
        {
          emitTest(L_Add(L_ZeroInitialiser(L_IntType(8)), L_ZeroInitialiser(L_IntType(8))))
        }
      }
      it("L_Array")
      {
        expect("%0 = add [3 x i32] [ i32 0, i32 1, i32 2 ], [ i32 0, i32 1, i32 2 ]")
        {
          val v1 : Int = 0
          val v2 : Int = 1
          val v3 : Int = 2
          val myarray = L_Array(List(v1, v2, v3))
          emitTest(L_Add(myarray, myarray))
        }
      }      
    }
    
}