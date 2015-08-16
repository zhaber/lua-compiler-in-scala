/**
 * Obr language implementation main program.
 *
 * This file is part of Slem.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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

package org.slem.example.obr

import org.slem.example.obr.ObrTree.ObrInt
import org.kiama.util.RegexCompiler
//import org.slem.example.obr.SyntaxAnalysis

/**
 * Obr language implementation compiler driver.
 */
class Driver extends SyntaxAnalysis with RegexCompiler[ObrInt] {

    import java.io.FileReader
    import org.kiama.util.Console
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging._
    import org.slem.IRTree._
    import org.slem.IRTreeEncoder
    import org.slem.example.obr.IRTransform._
    import org.slem.example.obr.SemanticAnalysis._

    /**
     * The usage message for an erroneous invocation.
     */
    val usage = "usage: scala org.slem.example.obr.Main file.obr"

    /**
     * Function to process the input that was parsed.  emitter is
     * used for output.  Return true if everything worked, false
     * otherwise.
     */
    def process (ast : ObrInt, console : Console, emitter : Emitter) : Boolean = 
    {
        val oldprogram = false
        val testprog = false
        val newprog = true

        ast->errors
        if (messagecount > 0) {
            report
            false
        } else {
            val targettree = ast->code
            //println(targettree)
            val e = new IRTreeEncoder(emitter)
            e.encodeTree(targettree)
            true
        }
        
    }
}

/**
 * Obr language implementation main program.
 */
object Main extends Driver
