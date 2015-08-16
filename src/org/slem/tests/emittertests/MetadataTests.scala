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
 
class MetadataSpec extends Spec {

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
    
    describe("Metadata Tests - Metadata Strings: ") {
      it("L_MetadataString - Attached To Instr") {
        expect("%0 = add i32 1, 2, !" + '"' + "metidn" + '"' + " !" + '"' + "metval" + '"')
        {
          val metidn = L_MetadataString("metidn")
          val metval = L_MetadataString("metval")
          var instr = L_Add(1,2)
          instr.mapMetadata(metidn, metval)
          emitTest(instr)
        }
      }
      it("L_MetadataString - Value Encoding") {
        expect("!" + '"' + "teststring" + '"')
        {
          val myMetString = L_MetadataString("teststring")
          vtest(myMetString)
        }
      }
    }
    
    describe("Metadata Tests - Metadata Nodes: ") {
      it("L_MetadataNode - Base Value") {
        val out : String = "!0 = metadata !{ i32 5, metadata !" + '"' + "teststr" + '"' + "}"
        expect(out)
        {
          val myMetString = L_MetadataString("teststr")
          val unnamedmet = L_MetadataNode(List(5, myMetString))
          mtest(unnamedmet)
        }
      }
      it("L_MetadataNode - Attached to Instr") {
        expect("%0 = add i32 1, 2, !0 !1")
        {
          val metone = L_MetadataNode(List(100))
          val mettwo = L_MetadataNode(List(200))
          var instr = L_Add(1,2)
          instr.mapMetadata(metone, mettwo)
          emitTest(instr)
        }
      }
    }
    
    describe("Metadata Tests - Named Metadata: ") {
      it("L_NamedMetadata - Base Value") {
        expect("!foo = !{ !0, !1}")
        {
          val metone = L_MetadataNode(List(100))
          val mettwo = L_MetadataNode(List(200))
          val namedmet = L_NamedMetadata("foo", List(metone, mettwo))
          mtest(namedmet)
        }
      }
      it("L_NamedMetadata - Attached to Instr") {
        expect("%0 = add i32 1, 2, !foo !foo2")
        {
          val metone = L_MetadataNode(List(100))
          val mettwo = L_MetadataNode(List(200))
          val namedmetone = L_NamedMetadata("foo", List(metone, mettwo))
          val namedmettwo = L_NamedMetadata("foo2", List(metone, mettwo))
          var instr = L_Add(1,2)
          instr.mapMetadata(namedmetone, namedmettwo)
          emitTest(instr)        
        }
      }
    }
}