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

import de.masitec.mscheme.environment.DynamicEnvironment
import de.masitec.mscheme.machine.StackFrame
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.nulls.shouldBeNull
import io.kotest.matchers.nulls.shouldNotBeNull
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.shouldBeSameInstanceAs
import io.kotest.matchers.types.shouldNotBeSameInstanceAs

/**
 * @author sielenk
 */
internal abstract class StackTest<T : IStack> : FunSpec() {
    private var _stack: T? = null

    val stack: T
        get() = _stack.shouldNotBeNull()

    abstract fun createStack(): T


    init {
        beforeEach {
            _stack = createStack()
        }

        afterEach {
            _stack = null
        }


        test("testCreateSomething") {
            createFrame() shouldNotBeSameInstanceAs createFrame()
            createFrame() shouldNotBeSameInstanceAs createFrame()
        }

        test("testStack") {
            stack.shouldNotBeNull()
        }

        test("testIsEmpty") {
            stack.isEmpty shouldBe true
        }

        test("testPush") {
            stack.push(createFrame())
            stack.isEmpty shouldBe false
        }

        test("testPop1") {
            stack.tryPop().shouldBeNull()
        }

        test("testPop2") {
            val stack = stack
            val frame1: StackFrame = createFrame()
            val frame2: StackFrame = createFrame()
            val frame3: StackFrame = createFrame()

            stack.push(frame1)
            stack.push(frame2)
            stack.push(frame3)

            stack.isEmpty shouldBe false
            stack.tryPop() shouldBeSameInstanceAs frame3
            stack.isEmpty shouldBe false
            stack.tryPop() shouldBeSameInstanceAs frame2
            stack.isEmpty shouldBe false
            stack.tryPop() shouldBeSameInstanceAs frame1
            stack.isEmpty shouldBe true
            stack.tryPop().shouldBeNull()
        }
    }

    companion object {
        fun createFrame(): StackFrame =
            StackFrame(
                DynamicEnvironment.create(),
            ) { _, _ -> }
    }
}
