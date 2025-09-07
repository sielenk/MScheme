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
package de.masitec.mscheme.tests

import junit.framework.TestCase
import de.masitec.mscheme.environment.Environment.Companion.getEmpty
import de.masitec.mscheme.exceptions.TypeError
import de.masitec.mscheme.values.*
import de.masitec.mscheme.values.ListFactory.create
import de.masitec.mscheme.values.ListFactory.createPair
import de.masitec.mscheme.values.ValueTraits.eq
import de.masitec.mscheme.values.ValueTraits.equal
import de.masitec.mscheme.values.ValueTraits.eqv
import de.masitec.mscheme.values.ValueTraits.isEmpty
import de.masitec.mscheme.values.ValueTraits.isFunction
import de.masitec.mscheme.values.ValueTraits.isList
import de.masitec.mscheme.values.ValueTraits.isPair
import de.masitec.mscheme.values.ValueTraits.isPort
import de.masitec.mscheme.values.ValueTraits.isScmBoolean
import de.masitec.mscheme.values.ValueTraits.isScmChar
import de.masitec.mscheme.values.ValueTraits.isScmNumber
import de.masitec.mscheme.values.ValueTraits.isScmString
import de.masitec.mscheme.values.ValueTraits.isScmVector
import de.masitec.mscheme.values.ValueTraits.isSymbol
import de.masitec.mscheme.values.ValueTraits.isTrue
import de.masitec.mscheme.values.ValueTraits.toConstPair
import de.masitec.mscheme.values.ValueTraits.toEnvironment
import de.masitec.mscheme.values.ValueTraits.toInputPort
import de.masitec.mscheme.values.ValueTraits.toList
import de.masitec.mscheme.values.ValueTraits.toOutputPort
import de.masitec.mscheme.values.ValueTraits.toScmChar
import de.masitec.mscheme.values.ValueTraits.toScmNumber
import de.masitec.mscheme.values.ValueTraits.toScmString
import de.masitec.mscheme.values.ValueTraits.toScmVector
import de.masitec.mscheme.values.ValueTraits.toStaticEnvironment
import de.masitec.mscheme.values.ValueTraits.toSymbol
import de.masitec.mscheme.values.functions.CallCCFunction
import java.io.StringReader
import java.io.StringWriter

class TestValue
    (name: String?) : TestCase(name) {
    override fun setUp() {
    }

    override fun tearDown() {
    }


    private fun countTypes(v: Any?): Int {
        var count = 0

        if (isList(v)) {
            ++count
        }

        if (isScmBoolean(v)) {
            ++count
        }
        if (isPair(v)) {
            ++count
        }
        if (isSymbol(v)) {
            ++count
        }
        if (isScmNumber(v)) {
            ++count
        }
        if (isScmChar(v)) {
            ++count
        }
        if (isScmString(v)) {
            ++count
        }
        if (isScmVector(v)) {
            ++count
        }
        if (isPort(v)) {
            ++count
        }
        if (isFunction(v)) {
            ++count
        }

        return count
    }

    private fun countCasts(v: Any?): Int {
        var count = 0

        try {
            toList(v)
            ++count
        } catch (e: TypeError) {
        }

        try {
            toConstPair(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            toSymbol(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            toScmNumber(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            toScmChar(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            toScmString(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            toScmVector(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            toInputPort(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            toOutputPort(v)
            ++count
        } catch (e: TypeError) {
        }

        try {
            toEnvironment(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            toStaticEnvironment(v)
            ++count
        } catch (e: TypeError) {
        }

        return count
    }

    private fun commonTests(v: Any?, castCount: Int = 1) {
        assertTrue(isTrue(v))

        TestCase.assertEquals(1, countTypes(v))
        TestCase.assertEquals(countCasts(v), castCount)
    }


    fun testFalse() {
        val False: Any = ValueTraits.FALSE

        assertFalse(isTrue(False))

        TestCase.assertEquals(1, countTypes(False))
        TestCase.assertEquals(0, countCasts(False))

        assertTrue(isScmBoolean(False))
    }

    fun testTrue() {
        val True: Any = ValueTraits.TRUE

        assertTrue(isTrue(True))

        TestCase.assertEquals(1, countTypes(True))
        TestCase.assertEquals(0, countCasts(True))

        assertTrue(isScmBoolean(True))
    }

    fun testEmpty() {
        val empty: Any = ListFactory.create()

        assertTrue(isTrue(empty))

        TestCase.assertEquals(1, countTypes(empty)) // List
        TestCase.assertEquals(1, countCasts(empty))

        assertTrue(isEmpty(empty))

        assertTrue(isList(empty))
        assertSame(toList(empty), empty)
    }

    fun testPair() {
        val pair: Any = createPair(
            ValueTraits.TRUE,
            ValueTraits.TRUE
        )

        commonTests(pair)
        assertTrue(isPair(pair))
        assertSame(toConstPair(pair), pair)
    }

    fun testList() {
        val list: Any = create(
            ValueTraits.TRUE
        )

        assertTrue(isTrue(list))

        TestCase.assertEquals(2, countTypes(list)) // List and Pair
        TestCase.assertEquals(2, countCasts(list))

        assertTrue(isPair(list))
        assertSame(toConstPair(list), list)

        assertTrue(isList(list))
        assertSame(toList(list), list)
    }

    fun testSymbol() {
        val symbol: Any = "test"

        commonTests(symbol)
        assertTrue(isSymbol(symbol))
        assertSame(toSymbol(symbol), symbol)
    }

    fun testFunction() {
        val function: Any = CallCCFunction

        commonTests(function, 0)
        assertTrue(isFunction(function))
    }

    fun testNumber() {
        val number: Any = ScmNumber.Companion.create(49875)

        commonTests(number)
        assertTrue(isScmNumber(number))
        assertSame(toScmNumber(number), number)
    }

    fun testChar() {
        val character: Any = ValueTraits.toScmChar('a')

        commonTests(character)
        assertTrue(isScmChar(character))
        assertSame(toScmChar(character), character)
    }

    fun testString() {
        val string: Any = ScmString.Companion.create("Hallo !")

        commonTests(string)
        assertTrue(isScmString(string))
        assertSame(toScmString(string), string)
    }

    fun testVector() {
        val vector: Any = ScmVector.Companion.create()

        commonTests(vector)
        assertTrue(isScmVector(vector))
        assertSame(toScmVector(vector), vector)
    }

    fun testOutputPort() {
        val port: Any = OutputPort.Companion.create(StringWriter())

        commonTests(port)
        assertTrue(isPort(port))
        assertSame(toOutputPort(port), port)
    }

    fun testInputPort() {
        val port: Any = InputPort.Companion.create(StringReader(""))

        commonTests(port)
        assertTrue(isPort(port))
        assertSame(toInputPort(port), port)
    }

    fun testEnvironment() {
        val environment: Any = getEmpty()

        assertTrue(isTrue(environment))

        TestCase.assertEquals(0, countTypes(environment))
        TestCase.assertEquals(1, countCasts(environment))

        assertSame(toEnvironment(environment), environment)
    }

    fun testStaticEnvironment() {
        val environment: Any = getEmpty().static

        assertTrue(isTrue(environment))

        TestCase.assertEquals(0, countTypes(environment))
        TestCase.assertEquals(1, countCasts(environment))

        assertSame(toStaticEnvironment(environment), environment)
    }


    private fun eqHelper(fst: Any?, snd: Any?): Int {
        val eq = eq(fst, snd)
        val eqv = eqv(fst, snd)
        val equal = equal(fst, snd)

        // reflexivity
        assertTrue(eq(fst, fst))
        assertTrue(eq(snd, snd))
        assertTrue(eqv(fst, fst))
        assertTrue(eqv(snd, snd))
        assertTrue(equal(fst, fst))
        assertTrue(equal(snd, snd))

        // symmetry
        TestCase.assertEquals(eq, eq(snd, fst))
        TestCase.assertEquals(eqv, eqv(snd, fst))
        TestCase.assertEquals(equal, equal(snd, fst))

        assertTrue(!eq or eqv) // aka. eq  -> eqv
        assertTrue(!eqv or equal) // aka. eqv -> equal

        if (eq) {
            return 3
        } else if (eqv) {
            return 2
        } else if (equal) {
            return 1
        } else {
            return 0
        }
    }

    fun testEq() {
        val u: Any = "u"
        val v: Any = "v"

        // eq equivalent values
        TestCase.assertEquals(3, eqHelper(v, v))

        TestCase.assertEquals(
            3, eqHelper(
                ValueTraits.TRUE,
                ValueTraits.TRUE
            )
        )

        TestCase.assertEquals(
            3, eqHelper(
                ValueTraits.FALSE,
                ValueTraits.FALSE
            )
        )

        TestCase.assertEquals(
            3, eqHelper(
                "a",
                "a"
            )
        )

        TestCase.assertEquals(
            3, eqHelper(
                ListFactory.create(),
                ListFactory.create()
            )
        )

        // eqv equivalent values
        assertTrue(
            eqHelper(
                ScmNumber.Companion.create(7123645),
                ScmNumber.Companion.create(7123645)
            ) >= 2
        )

        assertTrue(
            eqHelper(
                ValueTraits.toScmChar('u'),
                ValueTraits.toScmChar('u')
            ) >= 2
        )

        // equal equivalent but eqv unspec. values
        assertTrue(
            eqHelper(
                ScmVector.Companion.create(),
                ScmVector.Companion.create()
            ) >= 1
        )

        assertTrue(
            eqHelper(
                ScmVector.Companion.create(5, v),
                ScmVector.Companion.create(5, v)
            ) >= 1
        )

        assertTrue(
            eqHelper(
                ScmString.Companion.create(""),
                ScmString.Companion.create("")
            ) >= 1
        )

        assertTrue(
            eqHelper(
                ScmString.Companion.create("Hallo"),
                ScmString.Companion.create("Hallo")
            ) >= 1
        )

        // equal equivalent but eqv different values
        TestCase.assertEquals(
            1, eqHelper(
                createPair(v, v),
                createPair(v, v)
            )
        )

        // different values
        TestCase.assertEquals(
            0, eqHelper(
                ValueTraits.TRUE,
                ValueTraits.FALSE
            )
        )

        TestCase.assertEquals(
            0, eqHelper(
                "u",
                "v"
            )
        )

        TestCase.assertEquals(
            0, eqHelper(
                ScmVector.Companion.create(5, u),
                ScmVector.Companion.create(5, v)
            )
        )

        TestCase.assertEquals(
            0, eqHelper(
                ScmVector.Companion.create(7, v),
                ScmVector.Companion.create(5, v)
            )
        )

        TestCase.assertEquals(
            0, eqHelper(
                ScmString.Companion.create("Hallo 1"),
                ScmString.Companion.create("Hallo 2")
            )
        )
    }
}
