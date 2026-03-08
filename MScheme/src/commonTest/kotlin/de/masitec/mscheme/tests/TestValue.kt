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

import de.masitec.mscheme.environment.Environment
import de.masitec.mscheme.exceptions.TypeError
import de.masitec.mscheme.util.createStdOutWriter
import de.masitec.mscheme.util.createStringReader
import de.masitec.mscheme.values.*
import de.masitec.mscheme.values.functions.CallCCFunction
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.comparables.shouldBeGreaterThanOrEqualTo
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.shouldBeSameInstanceAs

class TestValue : FunSpec({
    fun countTypes(v: Any?): Int {
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

    fun countCasts(v: Any?): Int {
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

    fun commonTests(v: Any?, castCount: Int = 1) {
        ValueTraits.isTrue(v) shouldBe true

        countTypes(v) shouldBe 1
        countCasts(v) shouldBe castCount
    }


    test("false") {
        val False: Any = ValueTraits.FALSE

        ValueTraits.isTrue(False) shouldBe false

        countTypes(False) shouldBe 1
        countCasts(False) shouldBe 0

        ValueTraits.isScmBoolean(False) shouldBe true
    }

    test("true") {
        val True: Any = ValueTraits.TRUE

        ValueTraits.isTrue(True) shouldBe true

        countTypes(True) shouldBe 1
        countCasts(True) shouldBe 0

        ValueTraits.isScmBoolean(True) shouldBe true
    }

    test("empty list") {
        val empty: Any = ListFactory.create()

        ValueTraits.isTrue(empty) shouldBe true

        countTypes(empty) shouldBe 1
        countCasts(empty) shouldBe 1

        ValueTraits.isEmpty(empty) shouldBe true

        ValueTraits.isList(empty) shouldBe true
        ValueTraits.toList(empty) shouldBeSameInstanceAs empty
    }

    test("pair") {
        val pair: Any = ListFactory.createPair(
            ValueTraits.TRUE,
            ValueTraits.TRUE
        )

        commonTests(pair)
        ValueTraits.isPair(pair) shouldBe true
        ValueTraits.toConstPair(pair) shouldBeSameInstanceAs pair
    }

    test("list") {
        val list: Any = ListFactory.create(
            ValueTraits.TRUE
        )

        ValueTraits.isTrue(list) shouldBe true

        countTypes(list) shouldBe 2
        countCasts(list) shouldBe 2

        ValueTraits.isPair(list) shouldBe true
        ValueTraits.toConstPair(list) shouldBeSameInstanceAs list

        ValueTraits.isList(list) shouldBe true
        ValueTraits.toList(list) shouldBeSameInstanceAs list
    }

    test("symbol") {
        val symbol: Any = Symbol("test")

        commonTests(symbol)
        ValueTraits.isSymbol(symbol) shouldBe true
        ValueTraits.toSymbol(symbol) shouldBeSameInstanceAs symbol
    }

    test("function") {
        val function: Any = CallCCFunction

        commonTests(function, 0)
        ValueTraits.isFunction(function) shouldBe true
    }

    test("number") {
        val number: Any = ScmNumber.create(49875)

        commonTests(number)
        ValueTraits.isScmNumber(number) shouldBe true
        ValueTraits.toScmNumber(number) shouldBeSameInstanceAs number
    }

    test("char") {
        val character: Any = ValueTraits.toScmChar('a')

        commonTests(character)
        ValueTraits.isScmChar(character) shouldBe true
        ValueTraits.toScmChar(character) shouldBe character
    }

    test("string") {
        val string: Any = ScmString.create("Hallo !")

        commonTests(string)
        ValueTraits.isScmString(string) shouldBe true
        ValueTraits.toScmString(string) shouldBeSameInstanceAs string
    }

    test("vector") {
        val vector: Any = ScmVector.create()

        commonTests(vector)
        ValueTraits.isScmVector(vector) shouldBe true
        ValueTraits.toScmVector(vector) shouldBeSameInstanceAs vector
    }

    test("output port") {
        val port: Any = OutputPort.create(createStdOutWriter())

        commonTests(port)
        ValueTraits.isPort(port) shouldBe true
        ValueTraits.toOutputPort(port) shouldBeSameInstanceAs port
    }

    test("input port") {
        val port: Any = InputPort.create(createStringReader(""))

        commonTests(port)
        ValueTraits.isPort(port) shouldBe true
        ValueTraits.toInputPort(port) shouldBeSameInstanceAs port
    }

    test("environment") {
        val environment: Any = Environment.getEmpty()

        ValueTraits.isTrue(environment) shouldBe true

        countTypes(environment) shouldBe 0
        countCasts(environment) shouldBe 1

        ValueTraits.toEnvironment(environment) shouldBeSameInstanceAs environment
    }

    test("static environment") {
        val environment: Any = Environment.getEmpty().static

        ValueTraits.isTrue(environment) shouldBe true

        countTypes(environment) shouldBe 0
        countCasts(environment) shouldBe 1

        ValueTraits.toStaticEnvironment(environment) shouldBeSameInstanceAs environment
    }


    fun eqHelper(fst: Any?, snd: Any?): Int {
        val eq = ValueTraits.eq(fst, snd)
        val eqv = ValueTraits.eqv(fst, snd)
        val equal = ValueTraits.equal(fst, snd)

        // reflexivity
        ValueTraits.eq(fst, fst) shouldBe true
        ValueTraits.eq(snd, snd) shouldBe true
        ValueTraits.eqv(fst, fst) shouldBe true
        ValueTraits.eqv(snd, snd) shouldBe true
        ValueTraits.equal(fst, fst) shouldBe true
        ValueTraits.equal(snd, snd) shouldBe true

        // symmetry
        ValueTraits.eq(snd, fst) shouldBe eq
        ValueTraits.eqv(snd, fst) shouldBe eqv
        ValueTraits.equal(snd, fst) shouldBe equal

        (!eq or eqv) shouldBe true // aka. eq  -> eqv
        (!eqv or equal) shouldBe true // aka. eqv -> equal

        return when {
            eq -> 3
            eqv -> 2
            equal -> 1
            else -> 0
        }
    }

    test("eq/eqv/equal") {
        val u: Any = "u"
        val v: Any = "v"

        // eq equivalent values
        eqHelper(v, v) shouldBe 3

        eqHelper(
            ValueTraits.TRUE,
            ValueTraits.TRUE
        ) shouldBe 3

        eqHelper(
            ValueTraits.FALSE,
            ValueTraits.FALSE
        ) shouldBe 3

        eqHelper(
            "a",
            "a"
        ) shouldBe 3

        eqHelper(
            ListFactory.create(),
            ListFactory.create()
        ) shouldBe 3

        // eqv equivalent values
        eqHelper(
            ScmNumber.create(7123645),
            ScmNumber.create(7123645)
        ) shouldBeGreaterThanOrEqualTo 2

        eqHelper(
            ValueTraits.toScmChar('u'),
            ValueTraits.toScmChar('u')
        ) shouldBeGreaterThanOrEqualTo 2

        // equal equivalent but eqv unspec. values
        eqHelper(
            ScmVector.create(),
            ScmVector.create()
        ) shouldBeGreaterThanOrEqualTo 1

        eqHelper(
            ScmVector.create(5, v),
            ScmVector.create(5, v)
        ) shouldBeGreaterThanOrEqualTo 1

        eqHelper(
            ScmString.create(""),
            ScmString.create("")
        ) shouldBeGreaterThanOrEqualTo 1

        eqHelper(
            ScmString.create("Hallo"),
            ScmString.create("Hallo")
        ) shouldBeGreaterThanOrEqualTo 1

        // equal equivalent but eqv different values
        eqHelper(
            ListFactory.createPair(v, v),
            ListFactory.createPair(v, v)
        ) shouldBe 1

        // different values
        eqHelper(
            ValueTraits.TRUE,
            ValueTraits.FALSE
        ) shouldBe 0

        eqHelper(
            "u",
            "v"
        ) shouldBe 0

        eqHelper(
            ScmVector.create(5, u),
            ScmVector.create(5, v)
        ) shouldBe 0

        eqHelper(
            ScmVector.create(7, v),
            ScmVector.create(5, v)
        ) shouldBe 0

        eqHelper(
            ScmString.create("Hallo 1"),
            ScmString.create("Hallo 2")
        ) shouldBe 0
    }
})
