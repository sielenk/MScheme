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
package de.masitec.mscheme.environment

import de.masitec.mscheme.exceptions.SchemeRuntimeError
import de.masitec.mscheme.util.Arity
import de.masitec.mscheme.values.IList


class DynamicEnvironment private constructor(
    private val _globals: MutableList<Any?>,
    private val _frames: Array<Array<Any?>>
) {
    fun createChild(
        arity: Arity,
        frameSize: Int,
        values: IList
    ): DynamicEnvironment =
        create(this, arity, frameSize, values)

    fun assign(ref: Reference, value: Any?): Any? {
        val level = ref.level
        val index = ref.index

        var result: Any? = null

        if (level > 0) {
            result = _frames[level - 1][index]
            _frames[level - 1][index] = value
        } else {
            val size = _globals.size

            if (index < size) {
                result = _globals[index]
                _globals[index] = value
            } else {
                while (_globals.size < index) {
                    _globals.add(null)
                }
                _globals.add(index, value)
            }
        }

        return result ?: value
    }

    fun lookupNoThrow(ref: Reference): Any? {
        val level = ref.level
        val index = ref.index

        if (0 < level && level <= _frames.size) {
            val frame = _frames[level - 1]

            if (0 <= index && index < frame.size) {
                return frame[index]
            }
        } else if (0 <= index && index < _globals.size) {
            return _globals.elementAt(index)
        }

        return null
    }

    fun lookup(ref: Reference): Any =
        lookupNoThrow(ref)
            ?: throw SchemeRuntimeError(
                ref.symbol,
                "uninitialized variable"
            )

    companion object {
        private fun create(
            parent: DynamicEnvironment,
            size: Int
        ): DynamicEnvironment =
            DynamicEnvironment(
                parent._globals,
                parent._frames + Array<Array<Any?>>(size) { arrayOf() }
            )

        private fun create(
            parent: DynamicEnvironment,
            arity: Arity,
            frameSize: Int,
            values: IList
        ): DynamicEnvironment {
            val result: DynamicEnvironment =
                create(parent, frameSize)

            val frame = result._frames[result._frames.size - 1]
            var rest = values

            for (i in 0..<arity.min) {
                frame[i] = rest.head
                rest = rest.tail
            }

            if (arity.allowMore()) {
                frame[arity.min] = rest
            }

            return result
        }

        fun create(): DynamicEnvironment =
            DynamicEnvironment(
                mutableListOf(),
                arrayOf()
            )
    }
}
