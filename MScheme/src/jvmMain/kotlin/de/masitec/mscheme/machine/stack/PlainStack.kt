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

/*
 * Created on 02.01.2004
 */
package de.masitec.mscheme.machine.stack

import de.masitec.mscheme.machine.StackFrame

/**
 * @author sielenk
 */
internal class PlainStack private constructor(
    private var _stack: Array<StackFrame?>,
    private var _sp: Int
) : IStack {
    constructor() : this(INITIAL_STACK, 0)

    constructor(other: PlainStack) : this(
        other._stack.copyOf(other._sp), other._sp
    )

    fun getCopy(): PlainStack =
        PlainStack(this)

    override val isEmpty: Boolean
        get() = _sp == 0

    override fun tryPop(): StackFrame? {
        if (_sp == 0) return null

        val sp = --_sp
        val value = _stack[sp]

        _stack[sp] = null

        return value!!
    }

    override fun push(frame: StackFrame) {
        val size = _stack.size

        if (_sp >= size) {
            _stack = _stack.copyInto(arrayOfNulls(size * 2 + 1))
        }

        _stack[_sp++] = frame
    }


    companion object {
        private val INITIAL_STACK = arrayOf<StackFrame?>()
    }
}
