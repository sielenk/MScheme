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
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package de.masitec.mscheme.machine.stack

import de.masitec.mscheme.exceptions.StackEmptyException
import de.masitec.mscheme.machine.StackFrame

/**
 * @author sielenk
 */
internal class PlainStack(
    private var _stack: Array<StackFrame?>,
    private var _sp: Int
) : IStack {

    constructor() : this(INITIAL_STACK, 0)

    constructor(other: PlainStack) : this(
        other._stack.copyOf(), other._sp
    )

    fun getCopy(): PlainStack =
        PlainStack(this)

    override val isEmpty: Boolean
        get() = _sp <= 0

    override fun pop(): StackFrame {
        assertFull()
        return _stack[--_sp]!!
    }

    override fun push(frame: StackFrame) {
        if (_sp >= _stack.size) {
            enlarge()
        }

        _stack[_sp++] = frame
    }


    private fun assertFull() {
        if (isEmpty) {
            throw StackEmptyException()
        }
    }

    private fun enlarge() {
        val length = _stack.size
        val oldStack = _stack
        val newStack = Array(length * 2 + 1) { i ->
            oldStack.getOrNull(i)
        }

        _stack = newStack
    }

    companion object {
        private val INITIAL_STACK = arrayOf<StackFrame?>()
    }
}
