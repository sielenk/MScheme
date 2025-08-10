/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine.stack

import mscheme.machine.StackFrame
import java.util.Stack

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
            throw ArrayIndexOutOfBoundsException()
        }
    }

    private fun enlarge() {
        val length = _stack.size
        val oldStack = _stack
        val newStack = arrayOfNulls<StackFrame>(length * 2 + 1)

        System.arraycopy(oldStack, 0, newStack, 0, length)

        _stack = newStack
    }

    companion object {
        private val INITIAL_STACK = arrayOf<StackFrame?>()
    }
}