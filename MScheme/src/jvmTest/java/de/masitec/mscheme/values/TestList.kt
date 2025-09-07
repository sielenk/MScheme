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
package de.masitec.mscheme.values

import junit.framework.TestCase
import de.masitec.mscheme.exceptions.PairExpected
import de.masitec.mscheme.values.ListFactory.create
import de.masitec.mscheme.values.ValueTraits.toConstPair

class TestList
    (name: String?) : TestCase(name) {
    var firstElement: Any? = null

    var secondElement: Any? = null

    var lastElement: Any? = null

    var emptyList: IList? = null

    var occupiedList: IList? = null

    var occupiedListLength: Int = 0

    override fun setUp() {
        emptyList = ListFactory.create()

        firstElement = "x"
        secondElement = "y"
        lastElement = secondElement
        occupiedList = create(firstElement, secondElement)
        occupiedListLength = 2
    }

    override fun tearDown() {
        firstElement = null
        secondElement = null
        emptyList = null
        occupiedList = null
    }

    fun testTestValues() {
        assertTrue(occupiedListLength >= 2)

        assertNotSame(firstElement, secondElement)

        assertNotSame(firstElement, lastElement)
    }

    fun testEmptyIsUnique() {
        assertSame("empty isn't unique", emptyList, ListFactory.create())
    }

    fun testOccupiedList() {
        assertNotSame("occupied list equals (==) empty list", occupiedList, emptyList)

        assertSame(
            "toPair returned somethig wrong", toConstPair(
                occupiedList
            ).first, occupiedList!!.head
        )
    }

    fun testIsEmpty() {
        assertTrue(emptyList!!.isEmpty)

        assertFalse(occupiedList!!.isEmpty)
    }

    fun testGetLength() {
        TestCase.assertEquals(0, emptyList!!.length)

        TestCase.assertEquals(occupiedList!!.length, occupiedListLength)
    }

    fun testGetHead() {
        try {
            emptyList!!.head
            fail("PairExpected expected")
        } catch (e: PairExpected) {
        }

        assertSame("getHead failed", occupiedList!!.head, firstElement)
    }

    fun testGetTail() {
        try {
            assertNotNull(emptyList!!.tail)
            fail("PairExpected expected")
        } catch (e: PairExpected) {
        }

        assertSame("getTail failed", occupiedList!!.tail.head, secondElement)
    }

    fun testGetReversed() {
        assertSame("failed on emptyList", emptyList!!.getReversed(), emptyList)

        TestCase.assertEquals(
            "length mismatch ",
            occupiedList!!.getReversed().length,
            occupiedListLength
        )

        assertSame(
            "previous last isn't first now", occupiedList!!.getReversed()
                .head, lastElement
        )
    }

    fun testGetCodeList() {
        // ...
    }
}
