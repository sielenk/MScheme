/*
 * Copyright (C) 2025  Marvin H. Sielenkemper
 *
 * This file is part of MScheme.
 *
 * MScheme is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 *
 * MScheme is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with MScheme; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA  02111-1307, USA.
 */
package de.masitec.mscheme.syntax

import de.masitec.mscheme.code.Application
import de.masitec.mscheme.code.CompiledLambda
import de.masitec.mscheme.code.Sequence
import de.masitec.mscheme.compiler.Compiler
import de.masitec.mscheme.environment.StaticEnvironment
import de.masitec.mscheme.util.Arity
import de.masitec.mscheme.values.IList
import de.masitec.mscheme.values.ValueTraits

// *** let* ***
internal object LetStar : LetBase(Arity.atLeast(2)) {
    override fun checkedTranslate(
        compilationEnv: StaticEnvironment,
        arguments: IList
    ): Any? {
        // (let* ((<var> <init>) ...) <body>)

        val bindings = ValueTraits.toList(arguments.head)
        val body = arguments.tail

        return if (bindings.isEmpty) {
            // special handling because the helper won't
            // create a new environment in this case

            Application.create(
                arrayOf<Any?>(
                    CompiledLambda.create(
                        Arity.exactly(0), body, compilationEnv
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
    fun translate(outerEnvironment: StaticEnvironment, bindings: IList): Any? {
        if (bindings.isEmpty) {
            return Sequence.create(_body.getCompiledArray(outerEnvironment))
        } else {
            val binding = ValueTraits.toList(bindings.head)

            val formal = ValueTraits.toSymbol(binding.head)
            val init = binding.tail.head

            val innerEnvironment = outerEnvironment
                .createChild(formal)

            val innerBody = translate(innerEnvironment, bindings.tail)

            val lambda: Any = CompiledLambda.create(
                Arity.exactly(1),
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
