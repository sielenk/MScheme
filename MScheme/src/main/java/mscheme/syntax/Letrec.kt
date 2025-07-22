/* The translation function for Scheme's 'letrec'.
   Copyright (C) 2001  Marvin H. Sielenkemper

This file is part of MScheme.

MScheme is free software; you can redistribute it and/or modify 
it under the terms of the GNU General Public License as published by 
the Free Software Foundation; either version 2 of the License, 
or (at your option) any later version. 

MScheme is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details. 

You should have received a copy of the GNU General Public License
along with MScheme; see the file COPYING. If not, write to 
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA  02111-1307, USA. */
package mscheme.syntax

import mscheme.code.Application
import mscheme.code.CompiledLambda.Companion.create
import mscheme.code.Sequence
import mscheme.compiler.Compiler
import mscheme.environment.StaticEnvironment
import mscheme.exceptions.SchemeException
import mscheme.util.Arity.Companion.atLeast
import mscheme.util.Arity.Companion.exactly
import mscheme.values.IList
import mscheme.values.ValueTraits.toSymbol


// *** letrec ***
internal object Letrec : LetBase(atLeast(2)) {
    @Throws(SchemeException::class, InterruptedException::class)
    override fun checkedTranslate(
        compilationEnv: StaticEnvironment,
        arguments: IList
    ): Any {
        // (letrec ((<var> <init>) ...) <body>)

        val formalsInitsBody = splitArguments(arguments)
        var formals = formalsInitsBody[0]
        var inits = formalsInitsBody[1]
        val body = formalsInitsBody[2]

        val numberOfFormals = formals.length

        val bodyCompilationEnv = compilationEnv.createChild(formals)

        val compiledBody = body.getCompiledArray(bodyCompilationEnv)

        val compiledLetrec = arrayOfNulls<Any>(numberOfFormals + compiledBody.size)

        val compiler = Compiler(bodyCompilationEnv)

        // prepend the initialisations to the body
        var index = 0
        while (!formals.isEmpty) {
            val formal = toSymbol(formals.head)
            val init = inits.head

            compiledLetrec[index++] = Set.translate(
                bodyCompilationEnv.getReferenceFor(formal),
                compiler.getForceable(init)
            )

            formals = formals.tail
            inits = inits.tail
        }

        System.arraycopy(
            compiledBody,
            0,
            compiledLetrec,
            index,
            compiledBody.size
        )

        return Application.create(
            arrayOf<Any?>(
                create(
                    exactly(0),
                    bodyCompilationEnv.size,
                    Sequence.create(
                        compiledLetrec
                    )
                )
            )
        )
    }
}
