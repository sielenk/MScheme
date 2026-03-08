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
package de.masitec.mscheme.util

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class TestArity : FunSpec({
    test("testExactly") {
        val arity0 = Arity.exactly(0)
        val arity1 = Arity.exactly(1)
        val arity2 = Arity.exactly(2)

        arity0.isValid(0) shouldBe true
        arity0.isValid(1) shouldBe false
        arity0.isValid(2) shouldBe false

        arity1.isValid(0) shouldBe false
        arity1.isValid(1) shouldBe true
        arity1.isValid(2) shouldBe false

        arity2.isValid(0) shouldBe false
        arity2.isValid(1) shouldBe false
        arity2.isValid(2) shouldBe true
        arity2.isValid(3) shouldBe false
    }

    test("testAtLeast") {
        val arity0 = Arity.atLeast(0)
        val arity1 = Arity.atLeast(1)
        val arity2 = Arity.atLeast(2)

        arity0.isValid(0) shouldBe true
        arity0.isValid(1) shouldBe true
        arity0.isValid(2) shouldBe true

        arity1.isValid(0) shouldBe false
        arity1.isValid(1) shouldBe true
        arity1.isValid(2) shouldBe true

        arity2.isValid(0) shouldBe false
        arity2.isValid(1) shouldBe false
        arity2.isValid(2) shouldBe true
        arity2.isValid(3) shouldBe true
    }

    test("testInarity") {
        val arity = Arity.inRange(2, 3)

        arity.isValid(0) shouldBe false
        arity.isValid(1) shouldBe false
        arity.isValid(2) shouldBe true
        arity.isValid(3) shouldBe true
        arity.isValid(4) shouldBe false
    }
})
