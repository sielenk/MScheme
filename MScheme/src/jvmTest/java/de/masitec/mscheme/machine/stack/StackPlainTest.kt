/*
 * Created on 02.01.2004
 *
 */
package de.masitec.mscheme.machine.stack


/**
 * @author sielenk
 */
internal class StackPlainTest(name: String?) : StackTest(name) {
    private var _stack: PlainStack? = null

    override fun createStack(): IStack {
        return PlainStack().also { _stack = it }
    }

    fun testCopy() {
        val SIZE = 50

        for (i in 0..<SIZE) {
            _stack!!.push(createFrame())
        }

        val otherStack: IStack = _stack!!.getCopy()

        for (j in 0..<SIZE) {
            assertFalse(_stack!!.isEmpty)
            assertFalse(otherStack.isEmpty)

            assertSame(_stack!!.pop(), otherStack.pop())
        }

        assertTrue(_stack!!.isEmpty)
        assertTrue(otherStack.isEmpty)
    }
}
