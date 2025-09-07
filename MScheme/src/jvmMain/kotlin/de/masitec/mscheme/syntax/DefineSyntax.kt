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
import de.masitec.mscheme.compiler.Compiler
import de.masitec.mscheme.environment.Environment.Companion.getSchemeReportEnvironment
import de.masitec.mscheme.environment.StaticEnvironment
import de.masitec.mscheme.machine.Machine
import de.masitec.mscheme.util.Arity.Companion.exactly
import de.masitec.mscheme.values.IList
import de.masitec.mscheme.values.IPair
import de.masitec.mscheme.values.ListFactory.create
import de.masitec.mscheme.values.ValueTraits.toStaticEnvironment
import de.masitec.mscheme.values.ValueTraits.toSymbol
import de.masitec.mscheme.values.functions.ApplyFunction

internal class Macro(
    private val _transformer: Any?,
    private val _definitionEnv: StaticEnvironment
) : ITranslator {
    override fun translate(
        usageEnv: StaticEnvironment,
        arguments: IList
    ): Any? {
        // (apply tranformer def_env use_env args)

        val result = MACHINE.execute(
            Application.create(
                arrayOf(
                    APPLY,
                    _transformer,
                    _definitionEnv,
                    usageEnv,
                    arguments
                )
            )
        ) as IPair?

        return Compiler(
            toStaticEnvironment(result!!.first)
        ).getForceable(
            result.second
        )
    }

    companion object {
        val MACHINE: Machine =
            Machine(getSchemeReportEnvironment())

        private val APPLY: Any =
            ApplyFunction
    }
}

internal object DefineSyntax : CheckedTranslator(exactly(2)) {
    override fun checkedTranslate(
        compilationEnv: StaticEnvironment,
        arguments: IList
    ): Any {
        val symbol = toSymbol(arguments.head)
        val value = arguments.tail.head

        val macro = Macro(
            Macro.MACHINE.evaluate(value),
            compilationEnv
        )

        compilationEnv.defineSyntax(symbol, macro)
        Macro.MACHINE
            .environment
            .static
            .defineSyntax(symbol, macro)

        return create()
    }
}
