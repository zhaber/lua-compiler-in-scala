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
 
class BinopSpec extends Spec {

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
 
    describe("Binop - Basic: ") {
 
    it("L_Add") {
      expect("%0 = add i32 1, 2") 
	  { 
	    binTest(L_Add(1,2))
	  }
    }
    it("L_NSWAdd") {
      expect("%0 = add nsw i32 1, 2") 
	  { 
	    binTest(L_NSWAdd(1,2))
	  }
    } 
    it("L_NUWAdd") {
      expect("%0 = add nuw i32 1, 2") 
	  { 
	    binTest(L_NUWAdd(1,2))
	  }
    } 
    it("L_NUWNSWAdd") {
      expect("%0 = add nuw nsw i32 1, 2") 
	  { 
	    binTest(L_NUWNSWAdd(1,2))
	  }
    } 
    it("L_FAdd") {
      expect("%0 = fadd float 1.0, 2.0") 
	  { 
	    binTest(L_FAdd(L_Float("1.0"),L_Float("2.0")))
	  }
    } 
    it("L_Sub") {
      expect("%0 = sub i32 1, 2") 
	  { 
	    binTest(L_Sub(1,2))
	  }
    } 
    it("L_NSWSub") {
      expect("%0 = sub nsw i32 1, 2") 
	  { 
	    binTest(L_NSWSub(1,2))
	  }
    } 
    it("L_NUWSub") {
      expect("%0 = sub nuw i32 1, 2") 
	  { 
	    binTest(L_NUWSub(1,2))
	  }
    } 
    it("L_NUWNSWSub") {
      expect("%0 = sub nuw nsw i32 1, 2") 
	  { 
	    binTest(L_NUWNSWSub(1,2))
	  }
    } 	
    it("L_FSub") {
      expect("%0 = fsub float 1.0, 2.0") 
	  { 
	    binTest(L_FSub(L_Float("1.0"),L_Float("2.0")))
	  }
    } 	
    it("L_Mul") {
      expect("%0 = mul i32 1, 2") 
	  { 
	    binTest(L_Mul(1,2))
	  }
    } 	
    it("L_NSWMul") {
      expect("%0 = mul nsw i32 1, 2") 
	  { 
	    binTest(L_NSWMul(1,2))
	  }
    } 	
    it("L_NUWMul") {
      expect("%0 = mul nuw i32 1, 2") 
	  { 
	    binTest(L_NUWMul(1,2))
	  }
    } 	
    it("L_NUWNSWMul") {
      expect("%0 = mul nuw nsw i32 1, 2") 
	  { 
	    binTest(L_NUWNSWMul(1,2))
	  }
    } 
    it("L_FMul") {
      expect("%0 = fmul float 1.0, 2.0") 
	  { 
	    binTest(L_FMul(L_Float("1.0"),L_Float("2.0")))
	  }
    } 
    it("L_UDiv") {
      expect("%0 = udiv i32 1, 2") 
	  { 
	    binTest(L_UDiv(1,2))
	  }
    } 
    it("L_ExactUDiv") {
      expect("%0 = udiv exact i32 1, 2") 
	  { 
	    binTest(L_ExactUDiv(1,2))
	  }
    } 	
    it("L_SDiv") {
      expect("%0 = sdiv i32 1, 2") 
	  { 
	    binTest(L_SDiv(1,2))
	  }
    } 	
    it("L_ExactSDiv") {
      expect("%0 = sdiv exact i32 1, 2") 
	  { 
	    binTest(L_ExactSDiv(1,2))
	  }
    } 	
    it("L_FDiv") {
      expect("%0 = fdiv float 1.0, 2.0") 
	  { 
	    binTest(L_FDiv(L_Float("1.0"),L_Float("2.0")))
	  }
    } 	
    it("L_URem") {
      expect("%0 = urem i32 1, 2") 
	  { 
	    binTest(L_URem(1,2))
	  }
    } 
    it("L_SRem") {
      expect("%0 = srem i32 1, 2") 
	  { 
	    binTest(L_SRem(1,2))
	  }
    } 
    it("L_FRem") {
      expect("%0 = frem float 1.0, 2.0") 
	  { 
	    binTest(L_FRem(L_Float("1.0"),L_Float("2.0")))
	  }
    } 	
    it("L_Shl") {
      expect("%0 = shl i32 1, 2") 
	  { 
	    binTest(L_Shl(1,2))
	  }
    } 
    it("L_NUWShl") {
      expect("%0 = shl nuw i32 1, 2") 
	  { 
	    binTest(L_NUWShl(1,2))
	  }
    } 
    it("L_NSWShl") {
      expect("%0 = shl nsw i32 1, 2") 
	  { 
	    binTest(L_NSWShl(1,2))
	  }
    } 
    it("L_NUWNSWShl") {
      expect("%0 = shl nuw nsw i32 1, 2") 
	  { 
	    binTest(L_NUWNSWShl(1,2))
	  }
    } 
    it("L_LShr") {
      expect("%0 = lshr i32 1, 2") 
	  { 
	    binTest(L_LShr(1,2))
	  }
    } 
    it("L_ExactLShr") {
      expect("%0 = lshr exact i32 1, 2") 
	  { 
	    binTest(L_ExactLShr(1,2))
	  }
    } 	
    it("L_AShr") {
      expect("%0 = ashr i32 1, 2") 
	  { 
	    binTest(L_AShr(1,2))
	  }
    } 
    it("L_ExactAShr") {
      expect("%0 = ashr exact i32 1, 2") 
	  { 
	    binTest(L_ExactAShr(1,2))
	  }
    } 
    it("L_And") {
      expect("%0 = and i32 1, 2") 
	  { 
	    binTest(L_And(1,2))
	  }
    } 
    it("L_Or") {
      expect("%0 = or i32 1, 2") 
	  { 
	    binTest(L_Or(1,2))
	  }
    } 
    it("L_Xor") {
      expect("%0 = xor i32 1, 2") 
	  { 
	    binTest(L_Xor(1,2))
	  }
    } 
  }
}