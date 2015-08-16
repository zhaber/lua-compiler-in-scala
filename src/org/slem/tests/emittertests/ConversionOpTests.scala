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
 
class ConversionOpSpec extends Spec {

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
	
	describe("ConversionOp - Type Inference: ") {
	    it("L_Trunc") {
			expect("i16")
			{
				typeTest(L_Trunc(1,L_IntType(16)))
			}
		}
        it("L_ZExt") {
            expect("i64")
            {
                typeTest(L_ZExt(1,L_IntType(64)))
            }
        }
        it("L_SExt") {
            expect("i64")
            {
                typeTest(L_SExt(1,L_IntType(64)))
            }
        }
        it("L_FPTrunc") {
            expect("float")
            {
                typeTest(L_FPTrunc(L_Double("1.0"), L_FloatType()))
            }
        }
        it("L_FPExt") {
            expect("double")
            {
                typeTest(L_FPExt(L_Float("1.0"), L_DoubleType()))
            }
        }
        it("L_FPToUI") {
            expect("i32")
            {
                typeTest(L_FPToUI(L_Float("1.0"), L_IntType(32)))
            }
        }
        it("L_FPToSI") {
            expect("i32")
            {
                typeTest(L_FPToSI(L_Float("1.0"), L_IntType(32)))
            }
        }
        it("L_UIToFP") {
            expect("float")
            {
                typeTest(L_UIToFP(1, L_FloatType()))
            }
        }
        it("L_SIToFP") {
            expect("float")
            {
                typeTest(L_SIToFP(1, L_FloatType()))
            }
        }
        it("L_PtrToInt") {
            expect("i32")
            {
                typeTest(L_PtrToInt(L_NullPointer(L_IntType(64)), L_IntType(32)))
            }
        }
        it("L_IntToPtr") {
            expect("i64*")
            {
                typeTest(L_IntToPtr(1, L_PointerType(L_IntType(64))))
            }
        }
        it("L_Bitcast") {
            expect("i64")
            {
                typeTest(L_Bitcast(1, L_IntType(64)))
            }
        }
	}
 
    describe("ConversionOp - Basic: ") {
 
    it("L_Trunc") {
      expect("%0 = trunc i32 1 to i16") 
	  { 
	    binTest(L_Trunc(1,L_IntType(16)))
	  }
    }
    it("L_ZExt") {
      expect("%0 = zext i32 1 to i64")
      {
        binTest(L_ZExt(1,L_IntType(64)))
      }
    }
    it("L_SExt") {
      expect("%0 = sext i32 1 to i64")
      {
        binTest(L_SExt(1,L_IntType(64)))
      }
    }
    it("L_FPTrunc") {
      expect("%0 = fptrunc double 1.0 to float")
      {
        binTest(L_FPTrunc(L_Double("1.0"), L_FloatType()))
      }
    }
    it("L_FPExt") {
      expect("%0 = fpext float 1.0 to double")
      {
        binTest(L_FPExt(L_Float("1.0"), L_DoubleType()))
      }
    }
    it("L_FPToUI") {
      expect("%0 = fptoui float 1.0 to i32")
      {
        binTest(L_FPToUI(L_Float("1.0"), L_IntType(32)))
      }
    }
    it("L_FPToSI") {
      expect("%0 = fptosi float 1.0 to i32")
      {
        binTest(L_FPToSI(L_Float("1.0"), L_IntType(32)))
      }
    }
    it("L_UIToFP") {
      expect("%0 = uitofp i32 1 to float")
      {
        binTest(L_UIToFP(1, L_FloatType()))
      }
    }
    it("L_SIToFP") {
      expect("%0 = sitofp i32 1 to float")
      {
        binTest(L_SIToFP(1, L_FloatType()))
      }
    }
    it("L_PtrToInt") {
      expect("%0 = ptrtoint i64* null to i32")
      {
        binTest(L_PtrToInt(L_NullPointer(L_IntType(64)), L_IntType(32)))
      }
    }
    it("L_IntToPtr") {
      expect("%0 = inttoptr i32 1 to i64*")
      {
        binTest(L_IntToPtr(1, L_PointerType(L_IntType(64))))
      }
    }
    it("L_Bitcast") {
      expect("%0 = bitcast i32 1 to i64")
      {
        binTest(L_Bitcast(1, L_IntType(64)))
      }
    }
  }
}