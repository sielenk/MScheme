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

import junit.framework.TestCase
import de.masitec.mscheme.machine.StackFrame

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
internal abstract class StackTest(name: String?) : TestCase(name) {
    private var _stack: IStack? = null

    protected abstract fun createStack(): IStack


    override fun setUp() {
        super.setUp()
        _stack = createStack()
    }

    override fun tearDown() {
        super.tearDown()
        _stack = null
    }


    fun testCreateSomething() {
        assertNotSame(createFrame(), createFrame())
        assertNotSame(createFrame(), createFrame())
    }

    fun testStack() {
        assertNotSame(null, _stack)
    }

    fun testIsEmpty() {
        assertTrue(_stack!!.isEmpty)
    }

    fun testPush() {
        _stack!!.push(createFrame())

        assertFalse(_stack!!.isEmpty)
    }

    fun testPop1() {
        var actual: Throwable? = null

        try {
            _stack!!.pop()
        } catch (t: Throwable) {
            actual = t
        }

        assertNotNull(actual)
    }

    fun testPop2() {
        val frame1: StackFrame = createFrame()
        val frame2: StackFrame = createFrame()
        val frame3: StackFrame = createFrame()

        _stack!!.push(frame1)
        _stack!!.push(frame2)
        _stack!!.push(frame3)

        assertFalse(_stack!!.isEmpty)
        assertSame(frame3, _stack!!.pop())

        assertFalse(_stack!!.isEmpty)
        assertSame(frame2, _stack!!.pop())

        assertFalse(_stack!!.isEmpty)
        assertSame(frame1, _stack!!.pop())

        assertTrue(_stack!!.isEmpty)
    }

    companion object {
        @JvmStatic
        protected fun createFrame(): StackFrame {
            return StackFrame(null, null)
        }
    }
}
