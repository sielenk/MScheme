/*
 * The translation function for Scheme's 'define'. Copyright (C) 2001 Marvin H.
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

import mscheme.compiler.Compiler
import mscheme.environment.StaticEnvironment
import mscheme.exceptions.SchemeException
import mscheme.util.Arity
import mscheme.values.IList
import mscheme.values.ListFactory.prepend
import mscheme.values.ValueTraits.isPair
import mscheme.values.ValueTraits.toConstPair
import mscheme.values.ValueTraits.toSymbol

internal object Define : CheckedTranslator(Arity.atLeast(2)) {
    override fun preTranslate(compilationEnv: StaticEnvironment) {
    }

    override fun checkedTranslate(
        compilationEnv: StaticEnvironment, arguments: IList
    ): Any {
        if (isPair(arguments.head)) {
            //    (define (f x y) (+ x y))
            // -> (define f (lambda (x y) (+ x y)))
            val symbol = toSymbol(
                toConstPair(
                    arguments.head
                ).first
            )
            val formals = toConstPair(arguments.head).second
            val body = arguments.tail

            val ref = compilationEnv.define(symbol)
            compilationEnv.setStateDefinitionBody(symbol)
            try {
                return Set.translate(
                    ref, Lambda.translate(
                        compilationEnv, prepend(formals, body)
                    )
                )
            } finally {
                compilationEnv.setStateOpen(symbol)
            }
        } else {
            if (!arguments.tail.tail.isEmpty) {
                arityError(arguments, Arity.exactly(2))
            }

            val symbol = toSymbol(arguments.head)
            val value = arguments.tail.head

            val ref = compilationEnv.define(symbol)
            compilationEnv.setStateDefinitionBody(symbol)
            try {
                return Set.translate(
                    ref,
                    Compiler(compilationEnv).getForceable(value)
                )
            } finally {
                compilationEnv.setStateOpen(symbol)
            }
        }
    }
}
