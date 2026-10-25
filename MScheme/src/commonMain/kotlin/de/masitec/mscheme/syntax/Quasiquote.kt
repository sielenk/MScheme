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

import de.masitec.mscheme.code.Application
import de.masitec.mscheme.compiler.CompilationContext
import de.masitec.mscheme.exceptions.CompileError
import de.masitec.mscheme.util.Arity
import de.masitec.mscheme.values.Empty
import de.masitec.mscheme.values.IList
import de.masitec.mscheme.values.Symbol
import de.masitec.mscheme.values.ValueTraits
import de.masitec.mscheme.values.functions.Builtins


/**
 * Expands the template of a quasiquote expression into calls of
 * `cons`, `append` and `list->vector`. The functions are embedded
 * as values, so redefining them doesn't affect quasiquote. Parts
 * of the template without unquote stay constants, like with quote.
 */
internal object Quasiquote : CheckedTranslator(Arity.exactly(1)) {
    private val QUASIQUOTE = Symbol("quasiquote")
    private val UNQUOTE = Symbol("unquote")
    private val UNQUOTE_SPLICING = Symbol("unquote-splicing")

    private class Expansion(val code: Any?, val isConst: Boolean)

    override fun checkedTranslate(
        context: CompilationContext,
        arguments: IList
    ): Any? =
        expand(context.expression(), arguments.head, 1).code

    private fun expand(
        context: CompilationContext,
        template: Any?,
        depth: Int
    ): Expansion {
        if (ValueTraits.isScmVector(template)) {
            val elements = expand(
                context,
                ValueTraits.toScmVector(template).toList(),
                depth
            )

            return if (elements.isConst)
                constant(template)
            else
                call(Builtins.list_2D_3Evector, elements.code)
        }

        if (!ValueTraits.isPair(template)) {
            return constant(template)
        }

        val pair = ValueTraits.toConstPair(template)
        val head = pair.first

        if (isForm(template, UNQUOTE) || isForm(template, UNQUOTE_SPLICING)) {
            if (depth == 1) {
                if (head == UNQUOTE_SPLICING) {
                    throw CompileError(template, "unquote-splicing outside of a list")
                }

                return Expansion(
                    context.getForceable(argumentOf(template)),
                    false
                )
            }

            return combine(template, constant(head), expand(context, pair.second, depth - 1))
        }

        if (isForm(template, QUASIQUOTE)) {
            return combine(template, constant(head), expand(context, pair.second, depth + 1))
        }

        if (depth == 1 && isForm(head, UNQUOTE_SPLICING)) {
            val spliced = context.getForceable(argumentOf(head))
            val rest = expand(context, pair.second, depth)

            return call(Builtins.append, spliced, rest.code)
        }

        return combine(
            template,
            expand(context, head, depth),
            expand(context, pair.second, depth)
        )
    }

    private fun combine(template: Any?, first: Expansion, second: Expansion): Expansion =
        if (first.isConst && second.isConst)
            constant(template)
        else
            call(Builtins.cons, first.code, second.code)

    private fun constant(datum: Any?): Expansion =
        Expansion(ValueTraits.getConst(datum), true)

    private fun call(function: Any?, vararg arguments: Any?): Expansion =
        Expansion(Application.create(arrayOf(function, *arguments)), false)

    // (keyword argument)
    private fun isForm(o: Any?, keyword: Symbol): Boolean {
        if (!ValueTraits.isPair(o)) {
            return false
        }

        val pair = ValueTraits.toConstPair(o)
        val rest = pair.second

        return pair.first == keyword
                && ValueTraits.isPair(rest)
                && ValueTraits.toConstPair(rest).second is Empty
    }

    private fun argumentOf(form: Any?): Any? =
        ValueTraits.toConstPair(ValueTraits.toConstPair(form).second).first
}
