/*
 * The implementation of scheme's 'lambda'. Copyright (C) 2001 Marvin H.
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
package mscheme.code

import mscheme.code.Sequence.Companion.create
import mscheme.compiler.Compiler.Companion.force
import mscheme.compiler.IForceable
import mscheme.environment.DynamicEnvironment
import mscheme.environment.StaticEnvironment
import mscheme.exceptions.CompileError
import mscheme.exceptions.ListExpected
import mscheme.exceptions.PairExpected
import mscheme.exceptions.SchemeException
import mscheme.machine.Registers
import mscheme.util.Arity
import mscheme.values.IList
import mscheme.values.functions.CheckedFunction
import java.io.IOException
import java.io.Writer

class CompiledLambda private constructor(
    private val _arity: Arity,
    private val _frameSize: Int,
    private var _compiledBody: Any?
) : IForceable, IReduceable {
    internal inner class Closure(
        private val _enclosingEnvironment: DynamicEnvironment
    ) : CheckedFunction() {
        @Throws(IOException::class)
        fun write(destination: Writer) {
            destination.write("#[closure]")
        }

        override val arity: Arity
            get() = _arity

        @Throws(ListExpected::class, PairExpected::class)
        override fun checkedCall(state: Registers, args: IList): Any? {
            val newEnvironment = _enclosingEnvironment.createChild(
                _arity, _frameSize, args
            )

            state.environment = newEnvironment

            return _compiledBody
        }
    }

    @Throws(CompileError::class)
    override fun force(): Any? {
        _compiledBody = force(_compiledBody)
        return this
    }

    override fun toString(): String =
        "lambda:<$_compiledBody>"

    override fun reduce(state: Registers): Any =
        Closure(state.environment)

    companion object {
        @JvmStatic
        fun create(arity: Arity, frameSize: Int, compiledBody: Any?): CompiledLambda =
            CompiledLambda(arity, frameSize, compiledBody)

        @JvmStatic
        @Throws(SchemeException::class, InterruptedException::class)
        fun create(arity: Arity, body: IList, env: StaticEnvironment): CompiledLambda {
            val compiledBody = create(body.getCompiledArray(env))

            return create(arity, env.size, compiledBody)
        }
    }
}