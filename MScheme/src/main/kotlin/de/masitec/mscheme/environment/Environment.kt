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
package de.masitec.mscheme.environment

import de.masitec.mscheme.exceptions.AlreadyBound
import de.masitec.mscheme.exceptions.CompileError
import de.masitec.mscheme.exceptions.SchemeRuntimeError
import de.masitec.mscheme.syntax.TranslatorFactory
import de.masitec.mscheme.values.functions.getBuiltins
import java.io.IOException
import java.io.Writer

class Environment private constructor(
    val static: StaticEnvironment,
    val dynamic: DynamicEnvironment
) {
    fun writeOn(destination: Writer) {
        destination.write("#[environment]")
    }

    fun toEnvironment(): Environment {
        return this
    }

    // *** Environment access ************************************************

    // *** code access (compiletime) ***

    fun define(key: String, value: Any?): Reference {
        val newReference = static.define(key)
        assign(newReference, value)
        return newReference
    }

    // *** value access (runtime) ***
    fun assign(key: Reference, value: Any?): Any? =
        dynamic.assign(key, value)

    fun assign(key: String, value: Any?): Any? =
        assign(static.getReferenceFor(key), value)


    fun lookupNoThrow(ref: Reference): Any? =
        dynamic.lookupNoThrow(ref)

    fun lookup(ref: Reference): Any =
        lookupNoThrow(ref) ?: throw SchemeRuntimeError(
            ref.symbol,
            "uninitialized variable"
        )

    fun lookup(key: String): Any =
        lookup(static.getReferenceFor(key)) // ***********************************************************************

    companion object {
        private fun create(): Environment =
            Environment(
                StaticEnvironment.create(),
                DynamicEnvironment.create()
            )

        fun getEmpty(): Environment =
            create()

        fun getNullEnvironment(): Environment {
            val result: Environment =
                getEmpty()

            try {
                val staticBindings = result.static

                staticBindings.defineSyntax(
                    "quote",
                    TranslatorFactory.quoteToken
                )
                staticBindings.defineSyntax(
                    "if",
                    TranslatorFactory.ifToken
                )
                staticBindings.defineSyntax(
                    "begin",
                    TranslatorFactory.beginToken
                )
                staticBindings.defineSyntax(
                    "and",
                    TranslatorFactory.andToken
                )
                staticBindings.defineSyntax(
                    "or",
                    TranslatorFactory.orToken
                )
                staticBindings.defineSyntax(
                    "lambda",
                    TranslatorFactory.lambdaToken
                )
                staticBindings.defineSyntax(
                    "let",
                    TranslatorFactory.letToken
                )
                staticBindings.defineSyntax(
                    "let*",
                    TranslatorFactory.letStarToken
                )
                staticBindings.defineSyntax(
                    "letrec",
                    TranslatorFactory.letrecToken
                )
                staticBindings.defineSyntax(
                    "define",
                    TranslatorFactory.defineToken
                )
                staticBindings.defineSyntax(
                    "set!",
                    TranslatorFactory.setToken
                )
                staticBindings.defineSyntax(
                    "define-syntax",
                    TranslatorFactory.defineSyntaxToken
                )
            } catch (e: AlreadyBound) {
                throw RuntimeException(
                    "unexpected AlreadyBound in getNullEnvironment()"
                )
            }

            return result
        }

        fun getSchemeReportEnvironment(): Environment {
            val result: Environment =
                getNullEnvironment()

            try {
                for ((name, value) in getBuiltins()) {
                    result.define(name, value)
                }
            } catch (e: CompileError) {
                throw RuntimeException(
                    "unexpected CompileError"
                )
            }

            return result
        }
    }
}
