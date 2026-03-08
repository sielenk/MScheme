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
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.matchers.ints.shouldBeGreaterThanOrEqual
import io.kotest.matchers.nulls.shouldNotBeNull
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.shouldBeSameInstanceAs

/**
 * @author sielenk
 */
internal class StackListTest : StackTest<Stack>() {
    override fun createStack(): Stack =
        Stack()

    init {
        test("mark and reinstate") {
            val SIZE1 = 5
            val SIZE2 = 3
            val SIZE = SIZE1 + SIZE2

            SIZE1 shouldBeGreaterThanOrEqual 2
            SIZE2 shouldBeGreaterThanOrEqual 2

            val buffer = arrayOfNulls<StackFrame>(SIZE)

            var mark: Stack.Mark? = null

            for (i in 0..<SIZE) {
                if (i == SIZE1) {
                    mark = stack.createMark().shouldNotBeNull()
                }

                stack.push(createFrame().also { buffer[i] = it })
            }

            // a b c d e | f g h
            val slice = stack.cutSlice(mark!!).shouldNotBeNull()

            // a b c d e
            shouldThrow<Throwable> {
                stack.cutSlice(mark)
            }

            stack.tryPop() shouldBeSameInstanceAs buffer[SIZE1 - 1]!!

            // a b c d
            stack.reinstate(slice)
            // a b c d | f g h
            stack.tryPop() shouldBeSameInstanceAs buffer[SIZE - 1]!!

            // a b c d | f g
            stack.reinstate(slice)
            // a b c d | f g | f g h
            stack.tryPop() shouldBeSameInstanceAs buffer[SIZE - 1]!!

            // a b c d | f g | f g
            for (i in 1..<SIZE2) {
                stack.tryPop()
            }
            // a b c d | f g
            stack.tryPop() shouldBeSameInstanceAs buffer[SIZE - 2]!!

            // a b c d | f
            for (i in 2..<SIZE2) {
                stack.tryPop()
            }
            // a b c d
            stack.tryPop() shouldBeSameInstanceAs buffer[SIZE1 - 2]!!

            // a b c
            for (i in 2..<SIZE1) {
                stack.tryPop()
            }
            // -
            stack.isEmpty shouldBe true
        }
    }
}
