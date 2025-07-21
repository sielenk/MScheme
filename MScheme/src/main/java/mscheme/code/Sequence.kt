/* The implementation of Scheme's 'begin'.
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
package mscheme.code

import mscheme.code.CodeArray.force
import mscheme.code.CodeArray.printTuple
import mscheme.compiler.IForceable
import mscheme.exceptions.CompileError
import mscheme.machine.IContinuation
import mscheme.machine.Registers
import mscheme.syntax.ISequenceTags
import mscheme.values.ValueTraits.isTrue

class Sequence private constructor(
    private val _tag: Int,
    private val _sequence: Array<Any?>
) : ISequenceTags, IForceable, IReduceable {
    @Throws(CompileError::class)
    override fun force(): Any? {
        force(_sequence)
        return this
    }

    override fun toString(): String = "${
        when (_tag) {
            ISequenceTags.TAG_BEGIN -> "seq"
            ISequenceTags.TAG_AND -> "and"
            ISequenceTags.TAG_OR -> "or"
            else -> "error"
        }
    }:<${printTuple(_sequence)}>"

    override fun reduce(state: Registers): Any? =
        prepareNext(state, 0)

    private fun prepareNext(
        registers: Registers,
        index: Int
    ): Any? {
        if (index + 1 < _sequence.size) {
            registers.push(
                object : IContinuation {
                    override fun invoke(registers: Registers, value: Any?): Any? =
                        if (((_tag == ISequenceTags.TAG_AND) && !isTrue(value))
                            ||
                            ((_tag == ISequenceTags.TAG_OR) && isTrue(value))
                        ) {
                            value
                        } else {
                            prepareNext(registers, index + 1)
                        }
                })
        }

        return _sequence[index]
    }

    companion object {
        @JvmStatic
        fun create(tag: Int, sequence: Array<Any?>): Any? =
            when (sequence.size) {
                0 -> tag == ISequenceTags.TAG_AND
                1 -> sequence[0]
                else -> Sequence(tag, sequence)
            }

        @JvmStatic
        fun create(sequence: Array<Any?>): Any? =
            create(ISequenceTags.TAG_BEGIN, sequence)

        @JvmStatic
        fun createConj(sequence: Array<Any?>): Any? =
            create(ISequenceTags.TAG_AND, sequence)

        @JvmStatic
        fun createDisj(sequence: Array<Any?>): Any? =
            create(ISequenceTags.TAG_OR, sequence)
    }
}
