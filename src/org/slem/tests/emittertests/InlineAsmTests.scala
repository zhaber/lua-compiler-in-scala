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
 
class InlineAsmSpec extends Spec {

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
    
    describe("Inline Asm - Module level asm ") {
      it("L_AsmModule")
      {
        expect("module asm " + '"' + "asmstring" + '"' + "\n")
        {
          val module = L_AsmModule("asmstring")
          val program = L_Program(List(module))
          ptest(program)
        }
      }
    }
}