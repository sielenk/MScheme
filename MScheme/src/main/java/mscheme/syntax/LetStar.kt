/*
 * The translation function for Scheme's 'let*'. Copyright (C) 2001 Marvin H.
 * Sielenkemper
 *
 * This file is part of MScheme.
 *
 * MScheme is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * MScheme is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * MScheme; see the file COPYING. If not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
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
import mscheme.values.ValueTraits.toList
import mscheme.values.ValueTraits.toSymbol

// *** let* ***
internal object LetStar : LetBase(atLeast(2)) {
    @Throws(SchemeException::class, InterruptedException::class)
    override fun checkedTranslate(
        compilationEnv: StaticEnvironment,
        arguments: IList
    ): Any? {
        // (let* ((<var> <init>) ...) <body>)

        val bindings = toList(arguments.head)
        val body = arguments.tail

        return if (bindings.isEmpty) {
            // special handling because the helper won't
            // create a new environment in this case

            Application.create(
                arrayOf<Any?>(
                    create(
                        exactly(0), body, compilationEnv
                            .createChild()
                    )
                )
            )
        } else {
            LetStarHelper(body).translate(compilationEnv, bindings)
        }
    }
}

internal class LetStarHelper(private val _body: IList) {
    @Throws(SchemeException::class, InterruptedException::class)
    fun translate(outerEnvironment: StaticEnvironment, bindings: IList): Any? {
        if (bindings.isEmpty) {
            return Sequence.create(_body.getCompiledArray(outerEnvironment))
        } else {
            val binding = toList(bindings.head)

            val formal = toSymbol(binding.head)
            val init = binding.tail.head

            val innerEnvironment = outerEnvironment
                .createChild(formal)

            val innerBody = translate(innerEnvironment, bindings.tail)

            val lambda: Any = create(
                exactly(1),
                innerEnvironment.size, innerBody
            )

            return Application.create(
                arrayOf<Any?>(
                    lambda,
                    Compiler(outerEnvironment).getForceable(init)
                )
            )
        }
    }
}