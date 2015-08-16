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
 
class VectorSpec extends Spec {

    def emitTest(instr : L_Instruction) : String = 
    {
        val e = new StringEmitter()
		val encoder = new IRTreeEncoder(e)
		encoder.encodeInstruction(instr)
		e.result
    }
    
    def typeTest(typ : L_Type) : String =
    {
        val e = new StringEmitter()
        val encoder = new IRTreeEncoder(e)
        encoder.encodeType(typ)
        e.result
    }
	
    describe("Vector Operations - extractelement: ") {
      it("L_ExtractElement - Reference") {
        expect("%0 = extractelement <4 x i32>* %1, i32 0") 
	    {  
          val myvec = L_Alloca(L_VectorType(4, L_IntType(32)))
          val etest1 = emitTest(L_ExtractElement(myvec, 0))
          etest1
	    }
      }
      it("L_ExtractElement - Basic") {
        expect("%0 = extractelement <2 x i32> < i32 1, i32 2 >, i32 0") 
	    {  
          val myvec = L_Vector(List(1,2))
          val etest1 = emitTest(L_ExtractElement(myvec, 0))
          etest1
	    }
      }
      it("L_ExtractElement - Type Test") {
        expect("double") 
	    {  
          val myvec = L_Vector(List(1.0,2.0))
          val etest1 = typeTest(L_ExtractElement(myvec, 0)->resultType)
          etest1
	    }
      }  
      it("L_ExtractElement - Type Error Test") {
        expect("double") 
	    {  
          val myvec = 1.0
          val etest1 = typeTest(myvec->resultType)
          etest1
	    }
      }       
    }
    describe("Vector Operations - insertelement: ") {
      it("L_InsertElement - Reference") {
        expect("%0 = insertelement <4 x i32>* %1, i32 1, i32 0") 
	    {  
          val myvec = L_Alloca(L_VectorType(4, L_IntType(32)))
          val etest1 = emitTest(L_InsertElement(myvec, 1, 0))
          etest1
	    }
      }
      it("L_InsertElement - Basic") {
        expect("%0 = insertelement <2 x i32> < i32 1, i32 2 >, i32 1, i32 0") 
	    {  
          val myvec = L_Vector(List(1,2))
          val etest1 = emitTest(L_InsertElement(myvec, 1, 0))
          etest1
	    }        
      }
      it("L_InsertElement - Type Test") {
        expect("<2 x i32>") 
	    {  
          val myvec = L_Vector(List(1,2))
          val etest1 = typeTest(L_InsertElement(myvec, 1, 0)->resultType)
          etest1
	    }        
      }
    }
    describe("Vector Operations - shufflevector: ") {
      
      it("L_ShuffleVector - Basic") {
        expect("%0 = shufflevector <2 x double> < double 1.0, double 2.0 >, <2 x double> < double 3.0, double 4.0 >, <3 x i32> < i32 0, i32 1, i32 0 >") 
	    {  
          val vec1 = L_Vector(List(1.0,2.0))
          val vec2 = L_Vector(List(3.0,4.0))
          val vec3 = L_Vector(List(0,1,0))
	      emitTest(L_ShuffleVector(vec1, vec2, vec3))
	    }
      }
      it("L_ShuffleVector - Type Test") {
        expect("<3 x double>") 
	    {  
          val vec1 = L_Vector(List(1.0,2.0))
          val vec2 = L_Vector(List(3.0,4.0))
          val vec3 = L_Vector(List(0,1,0))
	      typeTest(L_ShuffleVector(vec1, vec2, vec3)->resultType)
	    }
      }   
      it("L_ShuffleVector - Type Test - Wrong Types") {
        expect("opaque") 
	    {  
          val vec1 = L_Vector(List(1.0,2.0))
          val vec2 = L_Vector(List(3.0,4.0))
          val vec3 = L_Vector(List(0,1,0))
	      typeTest(L_ShuffleVector(vec1, vec2, 1)->resultType)
	    }
      }     
      it("L_ShuffleVector - Type Test - Wrong Types 2") {
        expect("opaque") 
	    {  
          val vec1 = L_Vector(List(1.0,2.0))
          val vec2 = L_Vector(List(3.0,4.0))
          val vec3 = L_Vector(List(0,1,0))
	      typeTest(L_ShuffleVector(1, 2, vec3)->resultType)
	    }
      }        
    }
}