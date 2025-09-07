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
package de.masitec.mscheme.code

import de.masitec.mscheme.compiler.Compiler
import de.masitec.mscheme.compiler.IForceable
import de.masitec.mscheme.exceptions.CompileError
import de.masitec.mscheme.machine.IContinuation
import de.masitec.mscheme.machine.Registers
import de.masitec.mscheme.values.ValueTraits.isTrue

class Selection internal constructor(
    private val _test: Any?,
    private val _onTrue: Any?,
    private val _onFalse: Any?
) : IReduceable {
    override fun toString(): String =
        "sel:<$_test, $_onTrue, $_onFalse>"

    /* (non-Javadoc)
   * @see mscheme.code.Reduceable#reduce(mscheme.machine.Stack, mscheme.environment.DynamicEnvironment)
   */
    override fun reduce(state: Registers): Any? {
        state.push { _: Registers?, value: Any? ->
            if (isTrue(value))
                _onTrue
            else
                _onFalse
        }

        return _test
    }

    companion object {
        fun create(
            test: Any?,
            onTrue: Any?,
            onFalse: Any?
        ): Any =
            ForceableSelection(test, onTrue, onFalse)
    }
}

internal class ForceableSelection(
    private val _test: Any?,
    private val _onTrue: Any?,
    private val _onFalse: Any?
) : IForceable {
    override fun force(): Selection =
        Selection(
            Compiler.force(_test),
            Compiler.force(_onTrue),
            Compiler.force(_onFalse)
        )
}
