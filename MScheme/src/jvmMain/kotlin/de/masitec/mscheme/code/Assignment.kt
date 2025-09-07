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
import de.masitec.mscheme.environment.Reference
import de.masitec.mscheme.exceptions.CompileError
import de.masitec.mscheme.machine.Registers

class Assignment private constructor(
    private var _binding: Reference,
    private var _expression: Any?
) : IForceable, IReduceable {
    override fun force(): Assignment {
        _binding = _binding.forceRef()
        _expression = Compiler.force(_expression)
        return this
    }

    override fun toString(): String =
        "set:<$_binding, $_expression>"

    /* (non-Javadoc)
   * @see mscheme.code.Reduceable#reduce(mscheme.machine.Stack, mscheme.environment.DynamicEnvironment)
   */
    override fun reduce(state: Registers): Any? {
        state.push { state1: Registers, value: Any? ->
            state1.assign(
                _binding,
                value
            )
        }

        return _expression
    }

    companion object {
        fun create(
            binding: Reference,
            valueCalculation: Any?
        ): Assignment =
            Assignment(binding, valueCalculation)
    }
}
