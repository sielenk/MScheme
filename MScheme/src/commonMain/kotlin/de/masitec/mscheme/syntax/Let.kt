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
import de.masitec.mscheme.environment.StaticEnvironment
import de.masitec.mscheme.util.Arity
import de.masitec.mscheme.values.IList
import de.masitec.mscheme.values.ListFactory
import de.masitec.mscheme.values.ValueTraits
import de.masitec.mscheme.values.functions.YCombinator


// *** let ***
internal object Let : LetBase(Arity.atLeast(2)) {
    override fun checkedTranslate(
        compilationEnv: StaticEnvironment,
        arguments: IList
    ): Any {
        var arguments = arguments
        val name: String?

        if (ValueTraits.isSymbol(arguments.head)) {
            if (arguments.length < 3) {
                arityError(arguments)
            }
            // named let
            // (let <var> ((<var> <init>) ...) <body>)
            name = ValueTraits.toSymbol(arguments.head)
            arguments = arguments.tail
        } else {
            // (let ((<var> <init>) ...) <body>)
            name = null
        }

        val formalsInitsBody = splitArguments(arguments)
        var formals = formalsInitsBody[0]
        val inits = formalsInitsBody[1]
        val body = formalsInitsBody[2]

        if (name != null) {
            // for the named let, the usually anonymous
            // closure gets a name to be recursively callable.
            // to ensure this names uniqueness, it is prepended to
            // the formals list.
            formals = ListFactory.prepend(name, formals)
        }

        var compiledProc =
            Lambda.translate(
                compilationEnv,
                ListFactory.prepend(formals, body)
            )

        if (name != null) {
            // the "raw" closure of a named-let has one additional
            // argument, which is to be bound to the "curried"
            // closure -- the YCombinator does it's magic ...

            compiledProc = Application.create(
                arrayOf<Any?>(
                    YCombinator,
                    compiledProc
                )
            )
        }

        val compiledLet = inits.getCompiledArray(compilationEnv, 1)

        compiledLet[0] = compiledProc

        return Application.create(compiledLet)
    }
}
