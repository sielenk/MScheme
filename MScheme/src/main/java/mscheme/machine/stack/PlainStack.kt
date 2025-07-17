/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine.stack

import mscheme.machine.StackFrame

/**
 * @author sielenk
 */
internal class PlainStack : IStack {
    private var _stack: Array<StackFrame?>
    private var _sp: Int

    constructor(other: PlainStack) {
        _sp = other._sp
        _stack = arrayOfNulls<StackFrame>(_sp)

        System.arraycopy(other._stack, 0, _stack, 0, _sp)
    }

    constructor() {
        _sp = 0
        _stack = arrayOfNulls<StackFrame>(INITIAL_STACK_SIZE)
    }

    val copy: PlainStack
        get() = PlainStack(this)

    override fun isEmpty(): Boolean {
        return _sp <= 0
    }

    override fun pop(): StackFrame? {
        assertFull()
        return _stack[--_sp]
    }

    override fun push(frame: StackFrame?) {
        if (_sp >= _stack.size) {
            enlarge()
        }

        _stack[_sp++] = frame
    }


    private fun assertFull() {
        if (isEmpty()) {
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
        private const val INITIAL_STACK_SIZE = 0
    }
}