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

import de.masitec.mscheme.compiler.IForceable
import de.masitec.mscheme.machine.Registers
import de.masitec.mscheme.syntax.SequenceTags
import de.masitec.mscheme.values.ValueTraits


class Sequence private constructor(
    private val _tag: SequenceTags,
    private val _sequence: Array<Any?>
) : IForceable, IReduceable {
    override fun force(): Sequence {
        CodeArray.force(_sequence)
        return this
    }

    override fun toString(): String = "${
        when (_tag) {
            SequenceTags.BEGIN -> "seq"
            SequenceTags.AND -> "and"
            SequenceTags.OR -> "or"
        }
    }:<${CodeArray.printTuple(_sequence)}>"

    override fun reduce(state: Registers): Any? =
        prepareNext(state, 0)

    private fun prepareNext(
        registers: Registers,
        index: Int
    ): Any? {
        if (index + 1 < _sequence.size) {
            registers.push { registers, value ->
                if (((_tag == SequenceTags.AND) && !ValueTraits.isTrue(value))
                    ||
                    ((_tag == SequenceTags.OR) && ValueTraits.isTrue(value))
                ) {
                    value
                } else {
                    prepareNext(registers, index + 1)
                }
            }
        }

        return _sequence[index]
    }

    companion object {
        fun create(tag: SequenceTags, sequence: Array<Any?>): Any? =
            when (sequence.size) {
                0 -> tag == SequenceTags.AND
                1 -> sequence[0]
                else -> Sequence(tag, sequence)
            }

        fun create(sequence: Array<Any?>): Any? =
            create(SequenceTags.BEGIN, sequence)

        fun createConj(sequence: Array<Any?>): Any? =
            create(SequenceTags.AND, sequence)

        fun createDisj(sequence: Array<Any?>): Any? =
            create(SequenceTags.OR, sequence)
    }
}
