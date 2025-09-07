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

import de.masitec.mscheme.exceptions.SchemeRuntimeError
import de.masitec.mscheme.machine.StackFrame

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
internal class StackListTest(name: String?) : StackTest(name) {
    private var _stack: Stack? = null

    override fun createStack(): IStack {
        return Stack().also { _stack = it }
    }


    fun testMark() {
        val SIZE1 = 5
        val SIZE2 = 3
        val SIZE = SIZE1 + SIZE2

        assertTrue(SIZE1 >= 2)
        assertTrue(SIZE2 >= 2)

        val buffer = arrayOfNulls<StackFrame>(SIZE)

        var mark: Stack.Mark? = null

        for (i in 0..<SIZE) {
            if (i == SIZE1) {
                mark = _stack!!.createMark()
                assertNotNull(mark)
            }

            _stack!!.push(createFrame().also { buffer[i] = it })
        }

        // a b c d e | f g h
        val slice = _stack!!.cutSlice(mark!!)
        assertNotNull(slice)

        // a b c d e
        try {
            _stack!!.cutSlice(mark)
            fail()
        } catch (t: Throwable) {
        }

        assertSame(buffer[SIZE1 - 1], _stack!!.pop())

        // a b c d
        _stack!!.reinstate(slice)

        // a b c d | f g h
        assertSame(buffer[SIZE - 1], _stack!!.pop())

        // a b c d | f g
        _stack!!.reinstate(slice)

        // a b c d | f g | f g h
        assertSame(buffer[SIZE - 1], _stack!!.pop())

        // a b c d | f g | f g
        for (i in 1..<SIZE2) {
            _stack!!.pop()
        }

        // a b c d | f g
        assertSame(buffer[SIZE - 2], _stack!!.pop())

        // a b c d | f
        for (i in 2..<SIZE2) {
            _stack!!.pop()
        }

        // a b c d
        assertSame(buffer[SIZE1 - 2], _stack!!.pop())

        // a b c
        for (i in 2..<SIZE1) {
            _stack!!.pop()
        }

        // -
        assertTrue(_stack!!.isEmpty)
    }
}
