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
import de.masitec.mscheme.environment.Environment
import de.masitec.mscheme.exceptions.TypeError
import de.masitec.mscheme.util.Writer
import de.masitec.mscheme.values.*
import de.masitec.mscheme.values.ListFactory
import de.masitec.mscheme.values.ValueTraits
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

        if (ValueTraits.isList(v)) {
            ++count
        }

        if (ValueTraits.isScmBoolean(v)) {
            ++count
        }
        if (ValueTraits.isPair(v)) {
            ++count
        }
        if (ValueTraits.isSymbol(v)) {
            ++count
        }
        if (ValueTraits.isScmNumber(v)) {
            ++count
        }
        if (ValueTraits.isScmChar(v)) {
            ++count
        }
        if (ValueTraits.isScmString(v)) {
            ++count
        }
        if (ValueTraits.isScmVector(v)) {
            ++count
        }
        if (ValueTraits.isPort(v)) {
            ++count
        }
        if (ValueTraits.isFunction(v)) {
            ++count
        }

        return count
    }

    private fun countCasts(v: Any?): Int {
        var count = 0

        try {
            ValueTraits.toList(v)
            ++count
        } catch (e: TypeError) {
        }

        try {
            ValueTraits.toConstPair(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            ValueTraits.toSymbol(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            ValueTraits.toScmNumber(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            ValueTraits.toScmChar(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            ValueTraits.toScmString(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            ValueTraits.toScmVector(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            ValueTraits.toInputPort(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            ValueTraits.toOutputPort(v)
            ++count
        } catch (e: TypeError) {
        }

        try {
            ValueTraits.toEnvironment(v)
            ++count
        } catch (e: TypeError) {
        }
        try {
            ValueTraits.toStaticEnvironment(v)
            ++count
        } catch (e: TypeError) {
        }

        return count
    }

    private fun commonTests(v: Any?, castCount: Int = 1) {
        assertTrue(ValueTraits.isTrue(v))

        TestCase.assertEquals(1, countTypes(v))
        TestCase.assertEquals(countCasts(v), castCount)
    }


    fun testFalse() {
        val False: Any = ValueTraits.FALSE

        assertFalse(ValueTraits.isTrue(False))

        TestCase.assertEquals(1, countTypes(False))
        TestCase.assertEquals(0, countCasts(False))

        assertTrue(ValueTraits.isScmBoolean(False))
    }

    fun testTrue() {
        val True: Any = ValueTraits.TRUE

        assertTrue(ValueTraits.isTrue(True))

        TestCase.assertEquals(1, countTypes(True))
        TestCase.assertEquals(0, countCasts(True))

        assertTrue(ValueTraits.isScmBoolean(True))
    }

    fun testEmpty() {
        val empty: Any = ListFactory.create()

        assertTrue(ValueTraits.isTrue(empty))

        TestCase.assertEquals(1, countTypes(empty)) // List
        TestCase.assertEquals(1, countCasts(empty))

        assertTrue(ValueTraits.isEmpty(empty))

        assertTrue(ValueTraits.isList(empty))
        assertSame(ValueTraits.toList(empty), empty)
    }

    fun testPair() {
        val pair: Any = ListFactory.createPair(
            ValueTraits.TRUE,
            ValueTraits.TRUE
        )

        commonTests(pair)
        assertTrue(ValueTraits.isPair(pair))
        assertSame(ValueTraits.toConstPair(pair), pair)
    }

    fun testList() {
        val list: Any = ListFactory.create(
            ValueTraits.TRUE
        )

        assertTrue(ValueTraits.isTrue(list))

        TestCase.assertEquals(2, countTypes(list)) // List and Pair
        TestCase.assertEquals(2, countCasts(list))

        assertTrue(ValueTraits.isPair(list))
        assertSame(ValueTraits.toConstPair(list), list)

        assertTrue(ValueTraits.isList(list))
        assertSame(ValueTraits.toList(list), list)
    }

    fun testSymbol() {
        val symbol: Any = "test"

        commonTests(symbol)
        assertTrue(ValueTraits.isSymbol(symbol))
        assertSame(ValueTraits.toSymbol(symbol), symbol)
    }

    fun testFunction() {
        val function: Any = CallCCFunction

        commonTests(function, 0)
        assertTrue(ValueTraits.isFunction(function))
    }

    fun testNumber() {
        val number: Any = ScmNumber.create(49875)

        commonTests(number)
        assertTrue(ValueTraits.isScmNumber(number))
        assertSame(ValueTraits.toScmNumber(number), number)
    }

    fun testChar() {
        val character: Any = ValueTraits.toScmChar('a')

        commonTests(character)
        assertTrue(ValueTraits.isScmChar(character))
        assertSame(ValueTraits.toScmChar(character), character)
    }

    fun testString() {
        val string: Any = ScmString.create("Hallo !")

        commonTests(string)
        assertTrue(ValueTraits.isScmString(string))
        assertSame(ValueTraits.toScmString(string), string)
    }

    fun testVector() {
        val vector: Any = ScmVector.create()

        commonTests(vector)
        assertTrue(ValueTraits.isScmVector(vector))
        assertSame(ValueTraits.toScmVector(vector), vector)
    }

    fun testOutputPort() {
        val port: Any = OutputPort.create(Writer(StringWriter()))

        commonTests(port)
        assertTrue(ValueTraits.isPort(port))
        assertSame(ValueTraits.toOutputPort(port), port)
    }

    fun testInputPort() {
        val port: Any = InputPort.create(StringReader(""))

        commonTests(port)
        assertTrue(ValueTraits.isPort(port))
        assertSame(ValueTraits.toInputPort(port), port)
    }

    fun testEnvironment() {
        val environment: Any = Environment.getEmpty()

        assertTrue(ValueTraits.isTrue(environment))

        TestCase.assertEquals(0, countTypes(environment))
        TestCase.assertEquals(1, countCasts(environment))

        assertSame(ValueTraits.toEnvironment(environment), environment)
    }

    fun testStaticEnvironment() {
        val environment: Any = Environment.getEmpty().static

        assertTrue(ValueTraits.isTrue(environment))

        TestCase.assertEquals(0, countTypes(environment))
        TestCase.assertEquals(1, countCasts(environment))

        assertSame(ValueTraits.toStaticEnvironment(environment), environment)
    }


    private fun eqHelper(fst: Any?, snd: Any?): Int {
        val eq = ValueTraits.eq(fst, snd)
        val eqv = ValueTraits.eqv(fst, snd)
        val equal = ValueTraits.equal(fst, snd)

        // reflexivity
        assertTrue(ValueTraits.eq(fst, fst))
        assertTrue(ValueTraits.eq(snd, snd))
        assertTrue(ValueTraits.eqv(fst, fst))
        assertTrue(ValueTraits.eqv(snd, snd))
        assertTrue(ValueTraits.equal(fst, fst))
        assertTrue(ValueTraits.equal(snd, snd))

        // symmetry
        TestCase.assertEquals(eq, ValueTraits.eq(snd, fst))
        TestCase.assertEquals(eqv, ValueTraits.eqv(snd, fst))
        TestCase.assertEquals(equal, ValueTraits.equal(snd, fst))

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
                ScmNumber.create(7123645),
                ScmNumber.create(7123645)
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
                ScmVector.create(),
                ScmVector.create()
            ) >= 1
        )

        assertTrue(
            eqHelper(
                ScmVector.create(5, v),
                ScmVector.create(5, v)
            ) >= 1
        )

        assertTrue(
            eqHelper(
                ScmString.create(""),
                ScmString.create("")
            ) >= 1
        )

        assertTrue(
            eqHelper(
                ScmString.create("Hallo"),
                ScmString.create("Hallo")
            ) >= 1
        )

        // equal equivalent but eqv different values
        TestCase.assertEquals(
            1, eqHelper(
                ListFactory.createPair(v, v),
                ListFactory.createPair(v, v)
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
                ScmVector.create(5, u),
                ScmVector.create(5, v)
            )
        )

        TestCase.assertEquals(
            0, eqHelper(
                ScmVector.create(7, v),
                ScmVector.create(5, v)
            )
        )

        TestCase.assertEquals(
            0, eqHelper(
                ScmString.create("Hallo 1"),
                ScmString.create("Hallo 2")
            )
        )
    }
}
