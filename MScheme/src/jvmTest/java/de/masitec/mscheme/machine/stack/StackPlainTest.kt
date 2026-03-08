/*
 * Created on 02.01.2004
 *
 */
package de.masitec.mscheme.machine.stack

import io.kotest.matchers.shouldBe
import io.kotest.matchers.nulls.shouldBeNull
import io.kotest.matchers.types.shouldBeSameInstanceAs

/**
 * @author sielenk
 */
internal class StackPlainTest : StackTest<PlainStack>() {
    override fun createStack(): PlainStack =
        PlainStack()

    init {
        test("getCopy") {
            val SIZE = 50

            repeat(SIZE) {
                stack.push(createFrame())
            }

            val otherStack: IStack = stack.getCopy()

            repeat(SIZE) {
                stack.isEmpty shouldBe false
                otherStack.isEmpty shouldBe false

                stack.tryPop() shouldBeSameInstanceAs otherStack.tryPop()
            }

            stack.isEmpty shouldBe true
            otherStack.isEmpty shouldBe true
            stack.tryPop().shouldBeNull()
            otherStack.tryPop().shouldBeNull()
        }
    }
}
