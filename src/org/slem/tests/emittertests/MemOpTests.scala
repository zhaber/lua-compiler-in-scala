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
 
class MemOpSpec extends Spec {

    def emitTest(instr : L_Instruction) : String = 
    {
        val e = new StringEmitter()
		val encoder = new IRTreeEncoder(e)
		encoder.encodeInstruction(instr)
		e.result
    }
	def typeTest(instr : L_Instruction) : String =
	{
	    val e = new StringEmitter()
		val encoder = new IRTreeEncoder(e)
		encoder.encodeType(instr->resultType)
		e.result
	}	
    describe("Memory Op Instruction - alloca: ") {
 
      it("L_Alloca - i32") {
        expect("%0 = alloca i32") 
	    {  
	      emitTest(L_Alloca(L_IntType(32)))
	    }
      }
      it("L_Alloca - i8*") {
        expect("%0 = alloca i8*") 
	    {  
	      emitTest(L_Alloca(L_PointerType(L_IntType(8))))
	    }
      }
      it("L_Alloca - NumElements + alignment") {
        expect("%0 = alloca i8*, i32 5, align 2")
        {
          emitTest(L_Alloca(L_PointerType(L_IntType(8)), numElements = 5, alignment = 2))
        }
      }
    }
    
    describe("Memory Op Instruction - load: ") {
      
      it("L_Load - basic") {
        expect("%0 = load i32* %1")
        {
          val myPtr = L_Alloca(L_IntType(32))
          val stor = L_Store(myPtr, 1)
          emitTest(L_Load(L_PointerType(L_IntType(32)), myPtr))
        }
      }
      it("L_Load - aligned + volatile") {
        expect("%0 = volatile load i32* %1, align 5")
        {
          val myPtr = L_Alloca(L_IntType(32))
          val stor = L_Store(myPtr, 1)
          emitTest(L_Load(L_PointerType(L_IntType(32)), myPtr, alignment = 5, isVolatile = true))
        }
      } 
      it("L_Load - non-pointer type test - throw type error") {
        expect("opaque")
        {
          val myPtr = L_Alloca(L_IntType(32))
          val stor = L_Store(myPtr, 1)
          typeTest(L_Load(L_IntType(32), myPtr, alignment = 5, isVolatile = true))
        }
      }       
    }
    
    
    describe("Memory Op Instruction - store: ") {
      
      it("L_Store - basic") {
        expect("store i32 1, i32* %0")
        {
          val myPtr = L_Alloca(L_IntType(32))
          emitTest(L_Store(1, myPtr))
        }
      }
      
      it("L_Store - aligned + volatile")
      {
        expect("volatile store i32 1, i32* %0, align 5")
        {
          val myPtr = L_Alloca(L_IntType(32))
          emitTest(L_Store(1, myPtr, alignment = 5, isVolatile = true))
        }
      }
    }
    
    describe("Memory Op Instruction - getelementptr: ") {
      it("L_GetElementPtr - basic pointer")
      {
        expect("%0 = getelementptr i32* %1, i32 0")
        {
            val myPtr = L_Alloca(L_IntType(32))
            emitTest(L_GetElementPtr(L_PointerType(L_IntType(32)), myPtr, List(0)))
        }
      }
      it("L_GetElementPtr - basic pointer inbounds")
      {
        expect("%0 = getelementptr inbounds i32* %1, i32 0")
        {
            val myPtr = L_Alloca(L_IntType(32))
            emitTest(L_GetElementPtr(L_PointerType(L_IntType(32)), myPtr, List(0), inBounds = true))
        }
      }
      it("L_GetElementPtr - structure type test")
      {
        expect("i16*")
        {
            val mystr = L_Alloca(L_StructureType(List(L_IntType(64), L_IntType(32), L_IntType(16))))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 2)))
        }
      }
      it("L_GetElementPtr - packed structure type test")
      {
        expect("i16*")
        {
            val mystr = L_Alloca(L_PackedStructureType(List(L_IntType(64), L_IntType(32), L_IntType(16))))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 2)))
        }
      }
      it("L_GetElementPtr - nested structure type test")
      {
        expect("i16*")
        {
            val mystr = L_Alloca(L_StructureType(List(L_StructureType(List(L_IntType(64), L_IntType(32), L_IntType(16))), L_IntType(128))))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 0, 2)))
        }
      }
      it("L_GetElementPtr - nested packed structure type test")
      {
        expect("i16*")
        {
            val mystr = L_Alloca(L_PackedStructureType(List(L_PackedStructureType(List(L_IntType(64), L_IntType(32), L_IntType(16))), L_IntType(128))))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 0, 2)))
        }
      }
      it("L_GetElementPtr - nested structure type test 2")
      {
        expect("i128*")
        {
            val mystr = L_Alloca(L_StructureType(List(L_StructureType(List(L_IntType(64), L_IntType(32), L_IntType(16))), L_IntType(128))))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 1)))
        }
      }
      it("L_GetElementPtr - nested packed structure type test 2")
      {
        expect("i128*")
        {
            val mystr = L_Alloca(L_PackedStructureType(List(L_PackedStructureType(List(L_IntType(64), L_IntType(32), L_IntType(16))), L_IntType(128))))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 1)))
        }
      }
      it("L_GetElementPtr - vector type test")
      {
        expect("i16*")
        {
            val mystr = L_Alloca(L_VectorType(50, L_IntType(16)))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 0)))
        }
      }
      it("L_GetElementPtr - vector/structure type test 2")
      {
        expect("i64*")
        {
            val mystr = L_Alloca(L_VectorType(50, L_StructureType(List(L_IntType(16), L_IntType(64)))))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 0, 1)))
        }
      }
      it("L_GetElementPtr - array/vector/structure type test")
      {
        expect("i64*")
        {
            val mystr = L_Alloca(L_ArrayType(5, L_VectorType(50, L_StructureType(List(L_IntType(16), L_IntType(64))))))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 0, 0, 1)))
        }
      }
      it("L_GetElementPtr - array/vector/packed structure type test")
      {
        expect("i64*")
        {
            val mystr = L_Alloca(L_ArrayType(5, L_VectorType(50, L_PackedStructureType(List(L_IntType(16), L_IntType(64))))))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 0, 0, 1)))
        }
      }
      it("L_GetElementPtr - structure non-const index type test")
      {
        expect("opaque")
        {
            val nonint = L_Add(1, 2)
            val mystr = L_Alloca(L_ArrayType(5, L_VectorType(50, L_StructureType(List(L_IntType(16), L_IntType(64))))))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 0, 0, nonint)))
        }
      }
      it("L_GetElementPtr - packed structure non-const index type test")
      {
        expect("opaque")
        {
            val nonint = L_Add(1, 2)
            val mystr = L_Alloca(L_ArrayType(5, L_VectorType(50, L_PackedStructureType(List(L_IntType(16), L_IntType(64))))))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 0, 0, nonint)))
        }
      }
      it("L_GetElementPtr - array type test")
      {
        expect("i64*")
        {
            val mystr = L_Alloca(L_ArrayType(5, L_IntType(64)))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 1)))
        }
      }
      it("L_GetElementPtr - simple pointer")
      {
        expect("i64*")
        {
            val mystr = L_Alloca(L_IntType(64))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0)))
        }
      }
      it("L_GetElementPtr - empty index list")
      {
        expect("opaque")
        {
            val mystr = L_Alloca(L_IntType(64))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List()))
        }
      }      
      it("L_GetElementPtr - upreference")
      {
        expect("[5 x [6 x \\2]]*")
        {
            val mystr = L_Alloca(L_ArrayType(5, L_ArrayType(6, L_UpReferenceType(2))))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0,0,0)))
        }
      } 
      it("L_GetElementPtr - upreference test 2")
      {
        expect("[5 x \\2]*")
        {
            val mystr = L_Alloca(L_ArrayType(5, L_UpReferenceType(2)))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0,0,0)))
        }
      }  
      it("L_GetElementPtr - upreference test 3")
      {
        expect("\\1*")
        {
            val mystr = L_Alloca(L_ArrayType(5, L_UpReferenceType(1)))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0,0,0,0,0,0)))
        }
      }
      it("L_GetElementPtr - upreference test 4")
      {
        expect("\\1*")
        {
            val mystr = L_Alloca(L_VectorType(5, L_UpReferenceType(1)))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0,0)))
        }
      }
      it("L_GetElementPtr - upreference test 5")
      {
        expect("\\1*")
        {
            val mystr = L_Alloca(L_StructureType(List(L_UpReferenceType(1))))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0,0)))
        }
      }  
      it("L_GetElementPtr - upreference test 6")
      {
        expect("\\1*")
        {
            val mystr = L_Alloca(L_PackedStructureType(List(L_UpReferenceType(1))))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0,0)))
        }
      } 
      it("L_GetElementPtr - upreference test 7")
      {
        expect("\\1*")
        {
            val mystr = L_Alloca(L_PointerType(L_UpReferenceType(1)))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 0)))
        }
      }   
      it("L_GetElementPtr - upreference test 8")
      {
        expect("opaque")
        {
            val mystr = L_Alloca(L_PointerType(L_UpReferenceType(10)))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 0)))
        }
      }  
      it("L_GetElementPtr - upreference test 9")
      {
        expect("opaque")
        {
            val mystr = L_Alloca(L_PointerType(L_UpReferenceType(-1)))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0, 0)))
        }
      }    
      it("L_GetElementPtr - upreference test 10")
      {
        expect("opaque")
        {
            val mystr = L_Alloca(L_ArrayType(5, L_UpReferenceType(10)))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0,0,0)))
        }
      }    
      it("L_GetElementPtr - upreference test 11")
      {
        expect("opaque")
        {
            val mystr = L_Alloca(L_ArrayType(5, L_UpReferenceType(-1)))
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0,0,0)))
        }
      }   
      it("L_GetElementPtr - wrong type test")
      {
        expect("opaque")
        {
            val mystr = 1.0
            typeTest(L_GetElementPtr(mystr->resultType, mystr, List(0,0,0)))
        }
      }        
    }
    
    
    
}