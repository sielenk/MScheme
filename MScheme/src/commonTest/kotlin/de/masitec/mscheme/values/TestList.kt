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

import de.masitec.mscheme.exceptions.PairExpected
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.ints.shouldBeGreaterThanOrEqual
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.shouldBeSameInstanceAs
import io.kotest.matchers.types.shouldNotBeSameInstanceAs

class TestList : FunSpec({
    var firstElement: Any? = null

    var secondElement: Any? = null

    var lastElement: Any? = null

    var emptyList: IList? = null

    var occupiedList: IList? = null

    var occupiedListLength: Int = 0

    beforeEach {
        emptyList = ListFactory.create()

        firstElement = "x"
        secondElement = "y"
        lastElement = secondElement
        occupiedList = ListFactory.create(firstElement, secondElement)
        occupiedListLength = 2
    }

    afterEach {
        firstElement = null
        secondElement = null
        emptyList = null
        occupiedList = null
    }

    test("testTestValues") {
        occupiedListLength shouldBeGreaterThanOrEqual 2

        firstElement shouldNotBeSameInstanceAs secondElement

        firstElement shouldNotBeSameInstanceAs lastElement
    }

    test("testEmptyIsUnique") {
        emptyList shouldBeSameInstanceAs ListFactory.create()
    }

    test("testOccupiedList") {
        occupiedList shouldNotBeSameInstanceAs emptyList

        ValueTraits.toConstPair(occupiedList).first shouldBeSameInstanceAs occupiedList!!.head
    }

    test("testIsEmpty") {
        emptyList!!.isEmpty shouldBe true

        occupiedList!!.isEmpty shouldBe false
    }

    test("testGetLength") {
        emptyList!!.length shouldBe 0

        occupiedList!!.length shouldBe occupiedListLength
    }

    test("testGetHead") {
        shouldThrow<PairExpected> {
            emptyList!!.head
        }

        occupiedList!!.head shouldBeSameInstanceAs firstElement
    }

    test("testGetTail") {
        shouldThrow<PairExpected> {
            emptyList!!.tail
        }

        occupiedList!!.tail.head shouldBeSameInstanceAs secondElement
    }

    test("testGetReversed") {
        emptyList!!.getReversed() shouldBeSameInstanceAs emptyList

        occupiedList!!.getReversed().length shouldBe occupiedListLength

        occupiedList!!.getReversed().head shouldBeSameInstanceAs lastElement
    }

    test("testGetCodeList") {
        // ...
    }
})
