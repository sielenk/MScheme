/*
 * Copyright (C) 2026  Marvin H. Sielenkemper
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

import de.masitec.mscheme.code.CondArrow
import de.masitec.mscheme.code.Selection
import de.masitec.mscheme.code.Sequence
import de.masitec.mscheme.compiler.CompilationContext
import de.masitec.mscheme.exceptions.CompileError
import de.masitec.mscheme.util.Arity
import de.masitec.mscheme.values.IList
import de.masitec.mscheme.values.Symbol
import de.masitec.mscheme.values.ValueTraits


internal object Cond : CheckedTranslator(Arity.atLeast(1)) {
    private val ELSE = Symbol("else")
    private val ARROW = Symbol("=>")

    override fun checkedTranslate(
        context: CompilationContext,
        arguments: IList
    ): Any? =
        translateClauses(context.expression(), arguments)

    private fun translateClauses(
        context: CompilationContext,
        clauses: IList
    ): Any? {
        if (clauses.isEmpty) {
            return ValueTraits.FALSE
        }

        val clause = ValueTraits.toList(clauses.head)
        val test = clause.head
        val body = clause.tail

        if (test == ELSE) {
            if (!clauses.tail.isEmpty) {
                throw CompileError(clause, "else clause must be the last one")
            }
            if (body.isEmpty) {
                throw CompileError(clause, "empty else clause")
            }

            return Sequence.create(body.getCompiledArray(context))
        }

        val compiledTest = context.getForceable(test)

        if (body.isEmpty) {
            val rest = translateClauses(context, clauses.tail)

            return Sequence.createDisj(arrayOf(compiledTest, rest))
        }

        if (body.head == ARROW) {
            if (body.length != 2) {
                throw CompileError(clause, "=> needs exactly one receiver")
            }

            val compiledReceiver =
                context.getForceable(body.tail.head)
            val rest = translateClauses(context, clauses.tail)

            return CondArrow.create(compiledTest, compiledReceiver, rest)
        }

        val compiledBody =
            Sequence.create(body.getCompiledArray(context))
        val rest = translateClauses(context, clauses.tail)

        return Selection.create(compiledTest, compiledBody, rest)
    }
}
