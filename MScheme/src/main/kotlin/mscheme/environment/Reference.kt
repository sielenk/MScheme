/* A compiled Symbol, allows efficient access to environments.
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
package mscheme.environment

import mscheme.code.IReduceable
import mscheme.compiler.IForceable
import mscheme.exceptions.CompileError
import mscheme.machine.Registers

sealed class Reference protected constructor(
    val symbol: String
) : IForceable, IReduceable {
    override fun reduce(state: Registers): Any =
        state.environment.lookup(this)

    abstract val level: Int

    abstract val index: Int

    abstract fun forceRef(): Reference

    override fun force(): Reference =
        forceRef()

    override fun toString(): String =
        "ptr:<$level, $index, $symbol>"

    companion object {
        fun create(
            key: String,
            env: StaticEnvironment,
            restricted: Boolean
        ): Reference =
            DelayedReference(key, env, restricted)

        fun create(key: String, level: Int, index: Int): Reference =
            ForcedReference(key, level, index)
    }
}

internal class DelayedReference(
    key: String,
    private val _env: StaticEnvironment,
    private val _restricted: Boolean
) : Reference(key) {
    override val level: Int
        get() = throw RuntimeException(
            "$symbol delayed reference"
        )

    override val index: Int
        get() = throw RuntimeException(
            "$symbol delayed reference"
        )

    override fun forceRef(): Reference =
        _env.getReferenceFor(symbol, _restricted)
}

internal class ForcedReference(
    symbol: String,
    override val level: Int,
    override val index: Int
) : Reference(symbol) {
    override fun forceRef(): Reference =
        this
}
