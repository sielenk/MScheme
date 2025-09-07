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

import junit.framework.TestCase

class TestArity
    (name: String?) : TestCase(name) {
    fun testExactly() {
        val arity0 = Arity.exactly(0)
        val arity1 = Arity.exactly(1)
        val arity2 = Arity.exactly(2)

        assertTrue(arity0.isValid(0))
        assertFalse(arity0.isValid(1))
        assertFalse(arity0.isValid(2))

        assertFalse(arity1.isValid(0))
        assertTrue(arity1.isValid(1))
        assertFalse(arity1.isValid(2))

        assertFalse(arity2.isValid(0))
        assertFalse(arity2.isValid(1))
        assertTrue(arity2.isValid(2))
        assertFalse(arity2.isValid(3))
    }

    fun testAtLeast() {
        val arity0 = Arity.atLeast(0)
        val arity1 = Arity.atLeast(1)
        val arity2 = Arity.atLeast(2)

        assertTrue(arity0.isValid(0))
        assertTrue(arity0.isValid(1))
        assertTrue(arity0.isValid(2))

        assertFalse(arity1.isValid(0))
        assertTrue(arity1.isValid(1))
        assertTrue(arity1.isValid(2))

        assertFalse(arity2.isValid(0))
        assertFalse(arity2.isValid(1))
        assertTrue(arity2.isValid(2))
        assertTrue(arity2.isValid(3))
    }

    fun testInarity() {
        val arity = Arity.inRange(2, 3)

        assertFalse(arity.isValid(0))
        assertFalse(arity.isValid(1))
        assertTrue(arity.isValid(2))
        assertTrue(arity.isValid(3))
        assertFalse(arity.isValid(4))
    }
}
