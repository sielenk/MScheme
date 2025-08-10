/* The translation function for Scheme's 'define-syntax'.
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
import mscheme.compiler.Compiler
import mscheme.environment.Environment.Companion.getSchemeReportEnvironment
import mscheme.environment.StaticEnvironment
import mscheme.exceptions.SchemeException
import mscheme.machine.Machine
import mscheme.util.Arity.Companion.exactly
import mscheme.values.IList
import mscheme.values.IPair
import mscheme.values.ListFactory.create
import mscheme.values.ValueTraits.toStaticEnvironment
import mscheme.values.ValueTraits.toSymbol
import mscheme.values.functions.ApplyFunction

internal class Macro(
    private val _transformer: Any?,
    private val _definitionEnv: StaticEnvironment
) : ITranslator {
    @Throws(InterruptedException::class, SchemeException::class)
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
    @Throws(SchemeException::class, InterruptedException::class)
    override fun checkedTranslate(
        compilationEnv: StaticEnvironment,
        arguments: IList
    ): Any {
        val symbol = toSymbol(arguments.head)
        val value = arguments.tail.head

        val macro = Macro(
            Macro.Companion.MACHINE.evaluate(value),
            compilationEnv
        )

        compilationEnv.defineSyntax(symbol, macro)
        Macro.Companion.MACHINE
            .environment
            .static
            .defineSyntax(symbol, macro)

        return create()
    }
}
