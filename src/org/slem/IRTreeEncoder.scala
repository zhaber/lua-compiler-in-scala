/*
    Copyright 2010 Timothy Morton
    
    This file is part of Slem.

    Slem is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Slem is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Slem.  If not, see <http://www.gnu.org/licenses/>.
*/

package org.slem
import org.kiama.util.Emitter
import java.io.File

class IRTreeEncoder(emitter: Emitter) {
  import org.kiama.attribution.Attribution._
  import org.slem.IRTree._
  import java.io.FileWriter

  var debugmode = true
  var fileOutputEnabled = true
  var debugtreemode = false
  var fileout = ""

  var currentParamNum = 0
  var currentSSA = 0
  var currentGlobalNum = 0
  var currentFuncNum = 0
  var currentLabelNum = 0
  var currentMetadataNum = 0

  val metadataname: L_MetadataNode ==> String =
    {
      attr {
        case _ =>
          {
          getNewMetadataName()
        }
      }
    }

  val labelname: L_Label ==> String =
    {
      attr {
        case n: L_Label =>
          {
          if (n.label.size != 0) {
            n.label.label
          } else {
            getNewLabelName()
          }
        }
      }
    }

  val funcname: L_Function ==> String =
    {
      attr {
        case n: L_FunctionDefinition =>
          {
          if (n.funcName.size != 0) {
            "@" + n.funcName
          } else {
            getNewFuncName()
          }
        }
        case n: L_FunctionDeclaration =>
          {
          if (n.funcName.size != 0) {
            "@" + n.funcName
          } else {
            getNewFuncName()
          }
        }
        case _ =>
          {
          getNewFuncName()
        }
      }
    }

  val paramName: L_Argument ==> String =
    {
      attr {
        case n: L_Argument =>
          {
          if (n.argName.size == 0) {
            getNewParamName()
          } else {
            "%" + n.argName
          }
        }
      }
    }

  val ssa: L_Value ==> String =
    {
      attr {
        case _ => getNewSSA()
      }
    }

  val gvarname: L_GlobalVariable ==> String =
    {
      attr {
        case n: L_GlobalVariable =>
          {
          if (n.name.size == 0) {
            getNewGlobalVarName()
          } else {
            "@" + n.name
          }
        }
      }
    }

  def getNewLabelName(): String =
    {
      currentLabelNum = currentLabelNum + 1
      "block" + (currentLabelNum - 1)
    }

  def getNewFuncName(): String =
    {
      currentFuncNum = currentFuncNum + 1
      "@F" + (currentFuncNum - 1)
    }

  def getNewGlobalVarName(): String =
    {
      currentGlobalNum = currentGlobalNum + 1
      "@G" + (currentGlobalNum - 1)
    }

  def getNewParamName(): String =
    {
      currentParamNum = currentParamNum + 1
      "%param" + (currentParamNum - 1)
    }

  def getNewSSA(): String =
    {
      currentSSA = currentSSA + 1
      "%" + (currentSSA - 1)
    }

  def getNewMetadataName(): String =
    {
      currentMetadataNum = currentMetadataNum + 1
      "!" + (currentMetadataNum - 1)
    }

  def reset() =
    {
      fileout = ""
      currentParamNum = 0
      currentSSA = 0
    }

  def encodeTree(p: L_Program) =
    {
      reset()
      for (module <- p.modules) {
        module match
          {
            case m: L_Module =>
              {
              for (g <- m.globals) {
                encodeGlobal(g)
              }
              for (metnode <- m.metadata) {
                encodeMetadata(metnode)
                emitln()
              }
            }
            case m: L_AsmModule =>
              {
              emitln("module asm " + '"' + m.asm + '"')
            }
          }
      }
      if (fileOutputEnabled) {
        writeFile()
      }
    }

  def writeFile() = {
    val fw = new FileWriter("LLVMIR/newprog.ll")
    fw.write(fileout)
    fw.close()
  }

  def encodeGlobal(g: L_Global): Int =
    {
      g match
        {
          case v: L_GlobalVariable => encodeGlobalVariable(v)
          case f: L_FunctionReference => encodeGlobal(f.funcPtr)
          case f: L_FunctionDeclaration => encodeFunctionDeclaration(f)
          case f: L_FunctionDefinition => encodeFunctionDefinition(f)
          case _ => {}
        }
      0
    }

  def encodeArgument(a: L_Argument) =
    {
      encodeType(a.ty)
      a.ty match {
        case L_VarArgsType() => {}
        case _ =>
          emit(" ")
          if (a.value == null) {
            emit(a -> paramName)
          } else {
            encodeValue(a.value)
          }
      }

      for (attrib <- a.attrs) {
        emit(" ")
        emit(attrib)
      }
    }

  def encodeConstant(c: L_Constant) {
    c match
      {
        //Simple Constants
        case n: L_Boolean =>
          {
          if (n.value) {
            emit("true")
          } else {
            emit("false")
          }
        }
        case n: L_Int =>
          {
          emit("" + n.value)
        }
        case n: L_Float =>
          {
          emit(n.value)
        }
        case n: L_Double =>
          {
          emit(n.value)
        }

        case n: L_FP128 => emit(n.value)
        case n: L_X86FP80 => emit(n.value)
        case n: L_PPCFP128 => emit(n.value)

        /* No such thing as a pointer constant except for null ptr
            case n : L_Pointer =>
            {
                //encodeConstant(n.value)
                //emit("*")
                encodeValue(n.value)
                emit("*")
            }
            */
        case n: L_NullPointer =>
          {
          emit("null")
        }
        case n: L_Void =>
          {
        }
        //Complex Constants
        case n: L_PackedStructure =>
          {
          emit("< { ")
          var imax = n.elements.size
          var i = 1
          for (e <- n.elements) {
            encodeType(e -> resultType)
            emit(" ")
            encodeValue(e)
            if (i < imax) {
              emit(", ")
            }
            i = i + 1
          }
          emit(" } >")
        }
        case n: L_Structure =>
          {
          emit("{ ")
          var imax = n.elements.size
          var i = 1
          for (e <- n.elements) {
            encodeType(e -> resultType)
            emit(" ")
            encodeValue(e)
            if (i < imax) {
              emit(", ")
            }
            i = i + 1
          }
          emit(" }")
        }
        case n: L_Array =>
          {
          emit("[ ")
          var imax = n.elements.size
          var i = 1
          for (e <- n.elements) {
            encodeType(e -> resultType)
            emit(" ")
            encodeValue(e)
            if (i < imax) {
              emit(", ")
            }
            i = i + 1
          }
          emit(" ]")
        }
        case n: L_String =>
          {
          emit("c" + '"' + n.s + '"')
        }
        case n: L_ZeroInitialiser =>
          {
          emit("zeroinitializer")
        }
        case n: L_Vector =>
          {
          emit("< ")
          var imax = n.elements.size
          var i = 0
          for (e <- n.elements) {
            encodeType(e -> resultType)
            emit(" ")
            encodeValue(e)
            if (i < imax - 1) {
              emit(", ")
            }
            i = i + 1
          }
          emit(" >")
        }
        case n: L_BlockAddress =>
          {
          emit("blockaddress(")
          encodeValue(n.functionTarget)
          emit(", ")
          encodeLabel(n.blockTarget)
          emit(")")
        }
        case _ => emit("Unknown Constant")
      }
  }

  def encodeLabel(l: L_Label) =
    {
      emit(l -> labelname)
    }

  def encodeValue(v: L_Value): Int =
    {
      v match
        {
          case n: L_Instruction => emit(n -> ssa)
          case n: L_Argument => emit(n -> paramName)
          case n: L_GlobalVariable => emit(n -> gvarname)
          case n: L_Constant => encodeConstant(n)
          case n: L_FunctionReference => encodeValue(n.funcPtr)
          case n: L_FunctionDefinition => emit(n -> funcname)
          case n: L_FunctionDeclaration => emit(n -> funcname)
          case n: L_MetadataString => emit("!" + '"' + n.str + '"')
          case n: L_MetadataNode => emit(n -> metadataname)
          case n: L_NamedMetadata => emit("!" + n.name)
          case _ => emit("UnknownValue : " + v)
        }
      0
    }

  def encodeMetadata(m: L_BaseMetadata) {
    m match
      {
        case n: L_MetadataNode =>
          {
          emit(n -> metadataname + " = metadata !{ ")
          var i = 0
          for (mnode <- (n.fields)) {
            encodeType(mnode -> resultType)
            emit(" ")
            encodeValue(mnode)
            if (i < n.fields.size - 1) {
              emit(", ")
            }
            i = i + 1
          }
          emit("}")
        }
        case n: L_NamedMetadata =>
          {
          emit("!" + n.name + " = !{ ")
          var i = 0
          for (mnode <- (n.fields)) {
            //encodeType(mnode->resultType)
            //emit(" ")
            encodeValue(mnode)
            if (i < n.fields.size - 1) {
              emit(", ")
            }
            i = i + 1
          }
          emit("}")
        }
      }
  }

  def encodeBoundMetadata(b: L_Instruction) =
    {
      if (b.mappedMetadataIdn != null && b.mappedMetadataVal != null) {
        emit(", ")
        //encodeType(b.mappedMetadataIdn->resultType)
        //emit(" ")
        encodeValue(b.mappedMetadataIdn)
        emit(" ")
        //encodeType(b.mappedMetadataVal->resultType)
        //emit(" ")
        encodeValue(b.mappedMetadataVal)
      }
    }
  def encodeInstruction(b: L_Instruction) =
    {
      b match
        {
          case n: L_BinOpInstruction =>
            {
            emit(n -> ssa)
            emit(" = ")
            emitw(n.instructionString)
            encodeType(n.LHS -> resultType)
            emit(" ")
            encodeValue(n.LHS)
            emit(", ")
            encodeValue(n.RHS)

          }
          case n: L_ExtractElement =>
            {
            emit(n -> ssa)
            emit(" = extractelement ")
            encodeType(n.vec -> resultType)
            emit(" ")
            encodeValue(n.vec)
            emit(", i32 ")
            emit("")
            encodeValue(n.idx)
          }
          case n: L_InsertElement =>
            {
            emit(n -> ssa)
            emit(" = insertelement ")
            encodeType(n.vec -> resultType)
            emit(" ")
            encodeValue(n.vec)
            emit(", ")
            encodeType(n.elt -> resultType)
            emit(" ")
            encodeValue(n.elt)
            emit(", i32 ")
            encodeValue(n.idx)
          }
          case n: L_ShuffleVector =>
            {
            emit(n -> ssa)
            emit(" = shufflevector ")
            encodeType(n.v1 -> resultType)
            emit(" ")
            encodeValue(n.v1)
            emit(", ")
            encodeType(n.v2 -> resultType)
            emit(" ")
            encodeValue(n.v2)
            emit(", ")
            encodeType(n.mask -> resultType)
            emit(" ")
            encodeValue(n.mask)
          }
          case n: L_ExtractValue =>
            {
            emit(n -> ssa)
            emit(" = extractvalue ")
            encodeType(n.value -> resultType)
            emit(" ")
            encodeValue(n.value)
            for (idx <- n.indexes) {
              emit(", ") //FIXME changed code
              encodeValue(idx)
            }
          }
          case n: L_InsertValue =>
            {
            emit(n -> ssa)
            emit(" = insertvalue ")
            encodeType(n.value -> resultType)
            emit(" ")
            encodeValue(n.value)
            emit(", ")
            encodeType(n.elt -> resultType)
            emit(" ")
            encodeValue(n.elt)
            emit(", ")
            encodeValue(n.idx)
          }
          case n: L_Alloca =>
            {
            emit(n -> ssa)
            emit(" = alloca ")
            encodeType(n.typ)
            if (n.numElements != null) {
              emit(", ")
              encodeType(n.numElements -> resultType)
              emit(" ")
              encodeValue(n.numElements)
            }
            if (n.alignment != 0) {
              emit(", align " + n.alignment)
            }
          }
          case n: L_Load =>
            {
            emit(n -> ssa)
            emit(" = ")
            if (n.isVolatile) {
              emit("volatile ")
            }
            emit("load ")
            encodeType(n.typ)
            emit(" ")
            encodeValue(n.pointer)
            if (n.alignment != 0) {
              emit(", align " + n.alignment)
            }
            /* Use metadata nodes to support this
                if(n.nonTemporal)
                {
                    emit(", !nontemporal !" + n.nonTempIndex)
                }
                */
          }
          case n: L_Store =>
            {
            if (n.isVolatile) {
              emit("volatile ")
            }
            emitw("store")
            encodeType(n.value -> resultType)
            emit(" ")
            encodeValue(n.value)
            emit(", ")
            encodeType(n.pointer -> resultType)
            emit(" ")
            encodeValue(n.pointer)
            if (n.alignment != 0) {
              emit(", align " + n.alignment)
            }
            /* Use metadata nodes to support this
                if(n.nonTemporal)
                {
                    emit(", !nontemporal !" + n.nonTempIndex)
                }
                */
          }
          case n: L_GetElementPtr =>
            {
            emit(n -> ssa)
            emit(" = getelementptr ")
            if (n.inBounds) {
              emit("inbounds ")
            }
            encodeType(n.pty)
            emit(" ")
            encodeValue(n.pval)
            for (ti <- n.typeIndexes) {
              emit(", ")
              encodeType(ti -> resultType)
              emit(" ")
              encodeValue(ti)
              /*
                    encodeType(ti.ty)
                    emit(" ")// + ti.idx)
                    encodeValue(ti.idx)
                    */
            }
          }
          case n: L_ConversionOperation =>
            {
            emit(n -> ssa)
            emit(" = " + n.instructionString + " ")
            encodeType(n.value -> resultType)
            emit(" ")
            encodeValue(n.value)
            emit(" to ")
            encodeType(n.targetType)
          }
          case n: L_ICMP =>
            {
            emit(n -> ssa)
            emit(" = icmp " + n.compCode + " ")
            encodeType(n.LHS -> resultType)
            emit(" ")
            encodeValue(n.LHS)
            emit(", ")
            encodeValue(n.RHS)
          }
          case n: L_FCMP =>
            {
            emit(n -> ssa)
            emit(" = fcmp " + n.compCode + " ")
            encodeType(n.LHS -> resultType)
            emit(" ")
            encodeValue(n.LHS)
            emit(", ")
            encodeValue(n.RHS)
          }
          case n: L_Phi =>
            {
            emit(n -> ssa)
            emit(" = phi ")
            encodeType(n.valueLabels.head.value -> resultType)
            var i = 0
            for (vlab <- n.valueLabels) {
              emit(" [ ")
              encodeValue(vlab.value)
              emit(", %")
              encodeLabel(vlab.label)
              emit(" ]")
              if (i < n.valueLabels.size - 1) {
                emit(",")
              }
              i = i + 1
            }
          }
          case n: L_Select =>
            {
            emit(n -> ssa)
            emit(" = select ")
            encodeType(n.cond -> resultType)
            emit(" ")
            encodeValue(n.cond)
            emit(", ")
            encodeType(n.val1 -> resultType)
            emit(" ")
            encodeValue(n.val1)
            emit(", ")
            encodeType(n.val2 -> resultType)
            emit(" ")
            encodeValue(n.val2)
          }
          case n: L_Call =>
            {
            n.typ match {
              case L_VoidType() => {}
              case _ => {
                emit(n -> ssa)
                emit(" = ")
              }
            }
            if (n.tail) {
              emitw("tail")
            }
            emitw("call")
            if (n.callConvention.size > 0) {
              emitw(n.callConvention)
            }
            for (ra <- n.returnAttributes) {
              emitw(ra)
            }
            /*
                encodeType(n.typ)
                emit(" ")
                */
            if (n.fnty != null) {
              encodeType(n.fnty)
            } else {
              encodeType(n.fnptrval -> resultType)
            }
            emit(" ")

            encodeValue(n.fnptrval)
            var imax = n.fnargs.size
            var i = 1
            emit("( ")
            for (arg <- n.fnargs) {
              encodeArgument(arg)
              if (i < imax) {
                emit(", ")
              }
              i = i + 1
            }
            emit(" )")
            for (fnattr <- n.fnattrs) {
              emit(" " + fnattr)
            }
          }
          case n: L_Va_Arg =>
            {
            emit(n -> ssa)
            emit(" = va_arg ")
            encodeType(n.argList -> resultType)
            emit(" ")
            encodeValue(n.argList)
            emit(", ")
            encodeType(n.argType)
          }
          case _ => emit("Unknown Instruction : " + b)
        }
      encodeBoundMetadata(b)
    }

  def encodeTerminator(t: L_TerminatorInstruction) =
    {
      t match
        {
          case n: L_Ret =>
            {
            emitw("ret")
            n.rvalue -> resultType match
              {
                case n2: L_VoidType => emit("void")
                case _ =>
                  {
                  encodeType(n.rvalue -> resultType)
                  emit(" ")
                  encodeValue(n.rvalue)
                }
              }
          }
          case n: L_Br =>
            {
            emit("br label %")
            encodeLabel(n.dest)
          }
          case n: L_BrCond =>
            {
            emitw("br")
            encodeType(n.cond -> resultType)
            emit(" ")
            encodeValue(n.cond)
            emit(", label %")
            encodeLabel(n.ifTrue)
            emit(", label %")
            encodeLabel(n.ifFalse)
          }
          case n: L_Switch =>
            {
            emitw("switch")
            encodeType(n.value -> resultType)
            emit(" ")
            encodeValue(n.value)
            emit(", label %")
            encodeLabel(n.default)
            emit(" [ ")
            for (valLab <- n.cases) {
              encodeType(valLab.value -> resultType)
              emit(" ")
              encodeValue(valLab.value)
              emit(", label %")
              encodeLabel(valLab.label)
              emit(" ")
            }
            emit("]")
          }
          case n: L_IndirectBr =>
            {
            emitw("indirectbr")
            encodeType(n.address -> resultType)
            emit(" ")
            encodeValue(n.address)
            emit(", [ ")
            var labidx = 0
            for (lab <- n.possibleDestinations) {
              emit("label %")
              encodeLabel(lab)
              if (labidx < n.possibleDestinations.size - 1) {
                emit(",")
              }
              emit(" ")
              labidx = labidx + 1
            }
            emit("]")
          }
          case n: L_Invoke =>
            {
            emit(n -> ssa)
            emit(" = invoke ")
            if (n.callConv.size > 0) {
              emit(n.callConv)
              emit(" ")
            }
            for (ra <- n.retAttrs) {
              emit(ra + " ")
            }
            if (n.funcTypePtr != null) {
              encodeType(n.funcTypePtr)
            } else {
              encodeType(n.funcPtrVal -> resultType)
            }
            emit(" ")
            encodeValue(n.funcPtrVal)
            var imax = n.args.size
            var i = 1
            emit("( ")
            for (arg <- n.args) {
              encodeArgument(arg)
              if (i < imax) {
                emit(", ")
              }
              i = i + 1
            }
            emit(" )")

            for (at <- n.attrs) {
              emit(" " + at)
            }
            emit(" to label %")
            encodeLabel(n.normal)
            emit(" unwind label %")
            encodeLabel(n.unwind)
          }
          case n: L_Unwind =>
            {
            emit("unwind")
          }
          case n: L_Unreachable =>
            {
            emit("unreachable")
          }
          case _ => emit("Unknown Terminator Instruction : " + t)
        }
    }

  def encodeBlock(b: L_Block) =
    {
      encodeLabel(b.label)
      emitln(":")
      for (instr <- b.instructions) {
        emit("  ")
        encodeInstruction(instr)
        emitln()
      }
      emit("  ")
      encodeTerminator(b.terminator)
      emitln()
      emitln()
    }

  def encodeFunctionDefinition(f: L_FunctionDefinition) =
    {
      currentParamNum = 0
      currentSSA = 0
      emitw("define")
      emitw(f.linkage)
      emitw(f.visibilityStyle)
      emitw(f.callConvention)
      for (retattr <- f.returnAttributes) {
        emitw(retattr)
      }
      encodeType(f.returnType)
      emit(" ")
      emit(f -> funcname)
      emit("(")

      var imax = f.arguments.size
      var i = 1
      for (a <- (f.arguments)) {
        encodeArgument(a)
        if (i < imax) {
          emit(", ")
        }
        i = i + 1
      }

      emitw(")")
      for (funcAtt <- f.funcAttributes) {
        emitw(funcAtt)
      }
      if (f.section.size > 0) {
        emitw("section " + '"' + f.section + '"')
      }
      if (f.alignment != 0) {
        emitw("align " + f.alignment)
      }
      if (f.garbageCollector.size > 0) {
        emitw("gc " + '"' + f.garbageCollector + '"')
      }
      emitln("{")

      for (b <- f.blocks) {
        encodeBlock(b)
      }

      emitln("}")
      emitln("")
    }

  def encodeFunctionDeclaration(f: L_FunctionDeclaration) =
    {
      currentParamNum = 0
      currentSSA = 0
      emitw("declare")
      emitw(f.linkage)
      emitw(f.visibilityStyle)
      emitw(f.callConvention)
      for (retattr <- f.returnAttributes) {
        emitw(retattr)
      }
      encodeType(f.returnType)
      emit(" ")
      emit(f -> funcname)
      emit("(")

      var imax = f.arguments.size
      var i = 1
      for (a <- (f.arguments)) {
        encodeType(a -> resultType)
        if (i < imax) {
          emit(", ")
        }
        i = i + 1
      }

      emitw(")")
      if (f.alignment != 0) {
        emitw("align " + f.alignment)
      }
      if (f.garbageCollector.size > 0) {
        emitw("gc " + '"' + f.garbageCollector + '"')
      }
      emitln("")
    }

  def encodeType(t: L_Type): Int =
    {
      t match
        {
          //Basic types
          case n: L_IntType => emit("i" + n.size)
          case n: L_FloatType => emit("float")
          case n: L_DoubleType => emit("double")
          case n: L_FP128Type => emit("fp128")
          case n: L_X86FP80Type => emit("x86fp80")
          case n: L_PPCFP128Type => emit("ppcfp128")
          case n: L_VoidType => emit("void")
          case n: L_MetadataType => emit("metadata")
          case n: L_LabelType => emit("label")
          case n: L_VarArgsType => emit("...")

          //Derived types
          case n: L_ArrayType =>
            {
            emit("[" + n.numElements + " x ")
            encodeType(n.elementType)
            emit("]")
          }
          case n: L_FunctionType =>
            {
            encodeType(n.returnType)
            emit("( ")
            var imax = n.parameterList.size
            var i = 1
            for (param <- n.parameterList) {
              encodeType(param)
              if (i < imax) {
                emit(", ")
              }
              i = i + 1
            }
            emit(" )")
          }
          case n: L_StructureType =>
            {
            emit("{ ")
            var imax = n.fields.size
            var i = 0
            for (field <- n.fields) {
              encodeType(field)
              if (i < imax - 1) {
                emit(", ")
              }
              i = i + 1
            }
            emit(" }")
          }
          case n: L_PackedStructureType =>
            {
            emit("< { ")
            var imax = n.fields.size
            var i = 1
            for (field <- n.fields) {
              encodeType(field)
              if (i < imax) {
                emit(", ")
              }
              i = i + 1
            }
            emit(" } >")
          }
          case n: L_PointerType =>
            {
            encodeType(n.pointer)
            emit("*")
          }
          case n: L_VectorType =>
            {
            emit("<" + n.numElements + " x ")
            encodeType(n.elementType)
            emit(">")
          }
          case n: L_OpaqueType =>
            {
            emit("opaque")
          }
          case n: L_UpReferenceType =>
            {
            emit("\\" + n.levels)
          }
          case _ => "Unknown Type : " + t
        }
      0
    }

  def encodeGlobalVariable(g: L_GlobalVariable) =
    {
      emit(g -> gvarname)
      emit(" =")
      if (g.linkage.size > 0)
        emit(" " + g.linkage)
      emit(" global")
      if (g.addressSpace != 0) {
        emit(" addrspace(" + g.addressSpace + ")")
      }
      if (g.isConstant) {
        //emit(" constant")
      }
      emit(" ")
      encodeType(g.value -> resultType)
      emit(" ")
      encodeValue(g.value)
      if (g.section.size > 0) {
        emit(", section " + '"' + g.section + '"')
      }
      if (g.alignment != 0) {
        emit(", align " + g.alignment)
      }
      emitln()
      emitln()
    }

  def emitln() =
    {
      emit("\n")
    }

  def emitln(s: String) =
    {
      emit(s + "\n")
    }

  def emit(s: String) =
    {
      emitter.emit(s)
      if (fileOutputEnabled) {
        appendFile(s)
      }
    }

  def emitw(s: String) =
    {
      if (s.length > 0) {
        emit(s + " ")
      }
    }

  def appendFile(s: String) {
    fileout = fileout + s
  }

}