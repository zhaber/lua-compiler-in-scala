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
 
class OtherOpSpec extends Spec {

    def binTest(instr : L_Instruction) : String = 
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
	
	/*
    describe("BinOp - Type Inference: ") {
	    it("L_IntType") {
			expect("i32")
			{
				typeTest(L_Add(1,2))
			}
		}
		it("L_FloatType") {
			expect("float")
			{
				typeTest(L_Add(L_Float("1.0"), L_Float("2.0")))
			}
		}
		it("L_DoubleType") {
			expect("double")
			{
				typeTest(L_Add(L_Double("1.0"), L_Double("2.0")))
			}
		}
        it("L_VectorType") {
            expect("<3 x i32>")
			{
				val vec1 = L_Vector(List(0,1,2))
				val vec2 = L_Vector(List(3,4,5))
			    typeTest(L_Add(vec1, vec2))
			}
		}
	}
    */
 
    describe("ICMP - Basic: ") {
 
    it("L_ICmpEQ") {
      expect("%0 = icmp eq i32 1, 2") 
	  { 
	    binTest(L_ICmpEQ(1,2))
	  }
    }
    it("L_ICmpNE") {
      expect("%0 = icmp ne i32 1, 2") 
	  { 
	    binTest(L_ICmpNE(1,2))
	  }
    }
    it("L_ICmpNEQ") {
      expect("%0 = icmp ne i32 1, 2") 
	  { 
	    binTest(L_ICmpNEQ(1,2))
	  }
    }
    it("L_ICmpUGT") {
      expect("%0 = icmp ugt i32 1, 2") 
	  { 
	    binTest(L_ICmpUGT(1,2))
	  }
    }
    it("L_ICmpUGE") {
      expect("%0 = icmp uge i32 1, 2") 
	  { 
	    binTest(L_ICmpUGE(1,2))
	  }
    }
    it("L_ICmpULT") {
      expect("%0 = icmp ult i32 1, 2") 
	  { 
	    binTest(L_ICmpULT(1,2))
	  }
    }
    it("L_ICmpULE") {
      expect("%0 = icmp ule i32 1, 2") 
	  { 
	    binTest(L_ICmpULE(1,2))
	  }
    }
    it("L_ICmpSGT") {
      expect("%0 = icmp sgt i32 1, 2") 
	  { 
	    binTest(L_ICmpSGT(1,2))
	  }
    }
    it("L_ICmpSGE") {
      expect("%0 = icmp sge i32 1, 2") 
	  { 
	    binTest(L_ICmpSGE(1,2))
	  }
    }
    it("L_ICmpSLT") {
      expect("%0 = icmp slt i32 1, 2") 
	  { 
	    binTest(L_ICmpSLT(1,2))
	  }
    }
    it("L_ICmpSLE") {
      expect("%0 = icmp sle i32 1, 2") 
	  { 
	    binTest(L_ICmpSLE(1,2))
	  }
    }
    it("L_ICmp Result Type") {
      expect("i1")
      {
        typeTest(L_ICmpSLE(1,2))
      }
    }
  }
  describe("FCMP - Basic") {
    it("L_FCmpFalse") {
      expect("%0 = fcmp false float 1.0, 2.0")
      {
        binTest(L_FCmpFalse(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmpOEQ") {
      expect("%0 = fcmp oeq float 1.0, 2.0")
      {
        binTest(L_FCmpOEQ(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmpOGT") {
      expect("%0 = fcmp ogt float 1.0, 2.0")
      {
        binTest(L_FCmpOGT(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmpOGE") {
      expect("%0 = fcmp oge float 1.0, 2.0")
      {
        binTest(L_FCmpOGE(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmpOLT") {
      expect("%0 = fcmp olt float 1.0, 2.0")
      {
        binTest(L_FCmpOLT(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmpONE") {
      expect("%0 = fcmp one float 1.0, 2.0")
      {
        binTest(L_FCmpONE(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmpORD") {
      expect("%0 = fcmp ord float 1.0, 2.0")
      {
        binTest(L_FCmpORD(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmpUEQ") {
      expect("%0 = fcmp ueq float 1.0, 2.0")
      {
        binTest(L_FCmpUEQ(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmpUGT") {
      expect("%0 = fcmp ugt float 1.0, 2.0")
      {
        binTest(L_FCmpUGT(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmpUGE") {
      expect("%0 = fcmp uge float 1.0, 2.0")
      {
        binTest(L_FCmpUGE(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmpULT") {
      expect("%0 = fcmp ult float 1.0, 2.0")
      {
        binTest(L_FCmpULT(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmpULE") {
      expect("%0 = fcmp ule float 1.0, 2.0")
      {
        binTest(L_FCmpULE(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmpUNE") {
      expect("%0 = fcmp une float 1.0, 2.0")
      {
        binTest(L_FCmpUNE(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmpUNO") {
      expect("%0 = fcmp uno float 1.0, 2.0")
      {
        binTest(L_FCmpUNO(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmpTrue") {
      expect("%0 = fcmp true float 1.0, 2.0")
      {
        binTest(L_FCmpTrue(L_Float("1.0"), L_Float("2.0")))
      }
    }
    it("L_FCmp Result Type") {
      expect("i1")
      {
        typeTest(L_FCmpOEQ(1,2))
      }
    }
  }
  describe("Phi :") {
    it("L_Phi") {
      expect("%0 = phi i32 [ 1, %label1 ], [ 2, %label2 ], [ 3, %label3 ]")
      {
        val lab1 = L_Label("label1")
        val lab2 = L_Label("label2")
        val lab3 = L_Label("label3")
        val vlab1 = L_ValueLabel(1, lab1)
        val vlab2 = L_ValueLabel(2, lab2)
        val vlab3 = L_ValueLabel(3, lab3)
        binTest(L_Phi(List(vlab1, vlab2, vlab3)))
      }
    }
    it("L_Phi - TypeTest") {
      expect("i32")
      {
        val lab1 = L_Label("label1")
        val lab2 = L_Label("label2")
        val lab3 = L_Label("label3")
        val vlab1 = L_ValueLabel(1, lab1)
        val vlab2 = L_ValueLabel(2, lab2)
        val vlab3 = L_ValueLabel(3, lab3)
        typeTest(L_Phi(List(vlab1, vlab2, vlab3)))
      }
    } 
    it("L_Phi - TypeTest 2") {
      expect("opaque")
      {
        typeTest(L_Phi(List()))
      }
    } 
    
  }
  describe("Select :") {
    it("L_Select"){
      expect("%0 = select i1 1, i8 2, i8 3")
      {
        binTest(L_Select(L_Int(1,1), L_Int(8, 2), L_Int(8, 3)))
      }
    }
    it("L_Select - Type Test"){
      expect("i8")
      {
        typeTest(L_Select(L_Int(1,1), L_Int(8, 2), L_Int(8, 3)))
      }
    }
  }
  describe("Call :") {
    it("L_Call") {
      expect("%0 = call cconvtest retattrstest retattrstest2 i32( i32 )* @myfunc( i32 1 ) attrstest attrstest2")
      {
        val myfunc = L_FunctionDeclaration(L_IntType(32), funcName = "myfunc", arguments = List(L_IntType(32)))
        binTest(L_Call(L_IntType(32), myfunc, List(L_Int(32, 1)), callConvention = "cconvtest", returnAttributes = List("retattrstest", "retattrstest2"), fnattrs = List("attrstest", "attrstest2")))
      }      
    }
    it("L_Call - Type Test") {
      expect("i32")
      {
        val myfunc = L_FunctionDeclaration(L_IntType(32), funcName = "myfunc", arguments = List(L_IntType(32)))
        typeTest(L_Call(L_IntType(32), myfunc, List(L_Int(32, 1)), callConvention = "cconvtest", returnAttributes = List("retattrstest", "retattrstest2"), fnattrs = List("attrstest", "attrstest2")))
      }      
    }
  }
  describe("Va_Arg :") {
      it("L_Va_Arg") {
      expect("%0 = va_arg i8 1, i32")
      {
        binTest(L_Va_Arg(L_Int(8, 1), L_IntType(32)))  
      }
      expect("i32")
      {
        typeTest(L_Va_Arg(L_Int(8, 1), L_IntType(32)))  
      }
    }
  }
}