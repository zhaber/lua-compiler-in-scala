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
 
class AggregateSpec extends Spec {

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
	
    describe("Aggregate Operations: ") {
      it("L_ExtractValue") {
        expect("%0 = extractvalue { i32, double } { i32 1, double 1.4 }, i32 0") 
	    {  
          val mystr = L_Structure(List(1, 1.4))
          val etest1 = emitTest(L_ExtractValue(mystr, List(0)))
          etest1
	    }
      }
      it("L_ExtractValue - Structure Type Test") {
        expect("i32") 
	    {  
          val mystr = L_Structure(List(1, 1.4))
          val etest1 = typeTest(L_ExtractValue(mystr, List(0))->resultType)
          etest1
	    }     
        expect("double") 
	    {  
          val mystr = L_Structure(List(1, 1.4))
          val etest1 = typeTest(L_ExtractValue(mystr, List(1))->resultType)
          etest1
	    }
        expect("double")
        {
          val mystr = L_Structure(List(1, L_Structure(List(1.0))))
          val etest1 = typeTest(L_ExtractValue(mystr, List(1, 0))->resultType)
          etest1
        } 
        expect("i32")
        {
          val mystr = L_Structure(List(1, L_Structure(List(1.0))))
          val etest1 = typeTest(L_ExtractValue(mystr, List(0))->resultType)
          etest1
        }  
        expect("{ double, i32 }")
        {
          val mystr = L_Structure(List(1, L_Structure(List(1.0, 1))))
          val etest1 = typeTest(L_ExtractValue(mystr, List(1))->resultType)
          etest1
        } 
        expect("opaque")
        {
          val mystr = L_Structure(List(1, L_Structure(List(1.0))))
          val etest1 = typeTest(L_ExtractValue(mystr, List(999))->resultType)
          etest1
        }         
      }
      it("L_ExtractValue - Packed Structure Type Test") {
        expect("i32") 
	    {  
          val mystr = L_PackedStructure(List(1, 1.4))
          val etest1 = typeTest(L_ExtractValue(mystr, List(0))->resultType)
          etest1
	    }     
        expect("double") 
	    {  
          val mystr = L_PackedStructure(List(1, 1.4))
          val etest1 = typeTest(L_ExtractValue(mystr, List(1))->resultType)
          etest1
	    }
        expect("double")
        {
          val mystr = L_PackedStructure(List(1, L_PackedStructure(List(1.0))))
          val etest1 = typeTest(L_ExtractValue(mystr, List(1, 0))->resultType)
          etest1
        } 
        expect("i32")
        {
          val mystr = L_PackedStructure(List(1, L_PackedStructure(List(1.0))))
          val etest1 = typeTest(L_ExtractValue(mystr, List(0))->resultType)
          etest1
        }  
        expect("< { double, i32 } >")
        {
          val mystr = L_PackedStructure(List(1, L_PackedStructure(List(1.0, 1))))
          val etest1 = typeTest(L_ExtractValue(mystr, List(1))->resultType)
          etest1
        }
        expect("opaque")
        {
          val mystr = L_PackedStructure(List(1, L_PackedStructure(List(1.0))))
          val etest1 = typeTest(L_ExtractValue(mystr, List(999))->resultType)
          etest1
        }        
      }
      it("L_ExtractValue - Array Type Test") {
        expect("i32") 
	    {  
          val mystr = L_Array(List(1, 2))
          val etest1 = typeTest(L_ExtractValue(mystr, List(0))->resultType)
          etest1
	    }     
        expect("double") 
	    {  
          val mystr = L_Array(List(1.0, 2.0))
          val etest1 = typeTest(L_ExtractValue(mystr, List(1))->resultType)
          etest1
	    }
        expect("double")
	    {  
          val mystr = L_Array(List(L_Array(List(1.0)), L_Array(List(2.0))))
          val etest1 = typeTest(L_ExtractValue(mystr, List(1, 0))->resultType)
          etest1
	    }  
        expect("[1 x double]")
	    {  
          val mystr = L_Array(List(L_Array(List(1.0)), L_Array(List(2.0))))
          val etest1 = typeTest(L_ExtractValue(mystr, List(0))->resultType)
          etest1
	    }          
      }
      it("L_ExtractValue - Non Aggregate Type Test") {
        expect("opaque") 
	    {  
          val mystr = 1
          val etest1 = typeTest(L_ExtractValue(mystr, List(0))->resultType)
          etest1
	    } 
      }
      it("L_ExtractValue - Empty Indexes Type Test") {
        expect("opaque") 
	    {  
          val mystr = L_Array(List(L_Array(List(1.0)), L_Array(List(2.0))))
          val etest1 = typeTest(L_ExtractValue(mystr, List())->resultType)
          etest1
	    } 
      }
      it("L_InsertValue") {
        expect("%0 = insertvalue { i32, double } { i32 1, double 1.4 }, i32 2, 0")
        {
          val mystr = L_Structure(List(1, 1.4))
          val etest1 = emitTest(L_InsertValue(mystr, 2, 0))
          etest1
        }
      }
      it("L_InsertValue - Type Test") {
        expect("{ i32, double }")
        {
          val mystr = L_Structure(List(1, 1.4))
          val etest1 = typeTest(L_InsertValue(mystr, 2, 0)->resultType)
          etest1
        }
      }
      it("Packed Structures") {
        expect("%0 = insertvalue < { i32, double } > < { i32 1, double 1.4 } >, i32 2, 0")
        {
          val mystr = L_PackedStructure(List(1, 1.4))
          val etest1 = emitTest(L_InsertValue(mystr, 2, 0))
          etest1
        }
      }
   
    }


}