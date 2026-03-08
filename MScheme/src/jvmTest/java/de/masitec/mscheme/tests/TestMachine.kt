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

import de.masitec.mscheme.compiler.Compiler
import de.masitec.mscheme.environment.Environment
import de.masitec.mscheme.exceptions.*
import de.masitec.mscheme.machine.Machine
import de.masitec.mscheme.util.JvmReader
import de.masitec.mscheme.values.*
import de.masitec.mscheme.values.Function
import de.masitec.mscheme.values.ListFactory
import de.masitec.mscheme.values.ValueTraits
import de.masitec.mscheme.values.functions.CallCCFunction
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import io.kotest.matchers.shouldNotBe
import io.kotest.matchers.types.shouldBeInstanceOf
import io.kotest.matchers.types.shouldBeSameInstanceAs
import java.io.StringReader

class TestMachine : FunSpec({
    var machine: Machine? = null

    var _sym1: String? = null

    var _sym2: String? = null

    var _val1: Any? = null

    var _val2: Any? = null

    var _unval: Any? = null

    var _environment: Environment? = null

    beforeEach {
        _sym1 = "test1"
        _sym2 = "test2"

        _val1 = ValueTraits.TRUE
        _val2 = ValueTraits.FALSE
        _unval = ListFactory.create()

        _environment = Environment.getNullEnvironment()

        machine = Machine(_environment!!)
    }

    afterEach {
        machine = null
        _environment = null
    }

    fun evaluate(expression: String): Any? {
        return machine!!.evaluate(
            InputPort.create(JvmReader(StringReader(expression))).read()
        )
    }

    test("testTestValues") {
        val compiler = Compiler(_environment!!.static)

        shouldThrow<CantCompileException> {
            compiler.compile(_unval)
        }

        compiler.compile(_val1) shouldNotBe null
        compiler.compile(_val2) shouldNotBe null
    }

    test("testEnvironment") {
        machine!!.environment shouldBeSameInstanceAs _environment
    }

    test("testValue") {
        machine!!.evaluate(_val1) shouldBeSameInstanceAs _val1
        machine!!.evaluate(_val2) shouldBeSameInstanceAs _val2
    }

    test("testUnevaluatable") {
        shouldThrow<CantCompileException> {
            machine!!.evaluate(_unval)
        }
    }

    fun define(s: String, v: Any?) {
        _environment!!.define(s, v)
    }

    test("testSymbol") {
        shouldThrow<SchemeException> {
            machine!!.evaluate(_sym1)
        }

        define(_sym1!!, _val1)
        define(_sym2!!, _unval)

        machine!!.evaluate(_sym1) shouldBeSameInstanceAs _val1
        machine!!.evaluate(_sym2) shouldBeSameInstanceAs _unval
    }

    test("testPair1") {
        shouldThrow<ListExpected> {
            machine!!.evaluate(ListFactory.createPair(_val1, _val2))
        }
    }

    test("testPair2") {
        shouldThrow<FunctionExpected> {
            machine!!.evaluate(ListFactory.create(_val1))
        }
    }

    test("testQuote") {
        machine!!.evaluate(
            ListFactory.create(
                "quote",
                _unval
            )
        ) shouldBeSameInstanceAs _unval
    }

    test("testIf") {
        define(_sym1!!, _val1)
        define(_sym2!!, _val2)

        machine!!.evaluate(
            ListFactory.prepend(
                "if",
                ListFactory.create(ValueTraits.TRUE, _sym1, _sym2)
            )
        ) shouldBeSameInstanceAs _val1

        machine!!.evaluate(
            ListFactory.prepend(
                "if",
                ListFactory.create(ValueTraits.TRUE, _sym1)
            )
        ) shouldBeSameInstanceAs _val1

        machine!!.evaluate(
            ListFactory.prepend(
                "if",
                ListFactory.create(ValueTraits.FALSE, _sym1, _sym2)
            )
        ) shouldBeSameInstanceAs _val2
    }

    test("testBegin") {
        define(_sym2!!, _val2)

        shouldThrow<SchemeException> {
            machine!!.evaluate(
                ListFactory.create(
                    "begin", _sym1,
                    _sym2
                )
            )
        }

        _environment!!.define(_sym1!!, _val1)

        machine!!.evaluate(
            ListFactory.create("begin", _sym1, _sym2)
        ) shouldBeSameInstanceAs _val2
    }

    test("testLambdaFailures") {
        shouldThrow<CantCompileException> {
            evaluate("(lambda () #(1 2 3))")
        }

        shouldThrow<SymbolExpected> {
            evaluate("(lambda (#t) #t)")
        }

        shouldThrow<CompileError> {
            evaluate("(lambda (x y x) #t)")
        }
    }

    test("testLambdaNoArgs") {
        val func = machine!!.evaluate(
            ListFactory.create(
                "lambda",
                ListFactory.create(), _val1
            )
        ) as Function?

        machine!!.evaluate(ListFactory.create(func)) shouldBeSameInstanceAs _val1

        shouldThrow<CantCompileException> {
            machine!!.evaluate(ListFactory.create(func, _unval))
        }

        shouldThrow<RuntimeArityError> {
            machine!!.evaluate(ListFactory.create(func, _val1))
        }
    }

    test("testLambdaWithSimpleArgs") {
        val func = evaluate("(lambda (x y) x)") as Function?

        machine!!.evaluate(
            ListFactory.create(
                func, _val1,
                _val2
            )
        ) shouldBeSameInstanceAs _val1

        shouldThrow<RuntimeArityError> {
            machine!!.evaluate(ListFactory.create(func, _val1))
        }
    }

    test("testLambdaWithOptionalArgs") {
        val func = evaluate("(lambda (x . y) y)") as Function?

        shouldThrow<RuntimeArityError> {
            machine!!.evaluate(ListFactory.create(func))
        }

        machine!!.evaluate(
            ListFactory.create(
                func, _val1
            )
        ) shouldBeSameInstanceAs ListFactory.create()

        (machine!!.evaluate(
            ListFactory.create(
                func,
                _val1, _val2
            )
        ) as IList).head shouldBeSameInstanceAs _val2
    }

    test("testLambdaOptionalIsNewList") {
        val func = evaluate("(lambda x x)") as Function?

        val pair2: IPair =
            ListFactory.createPair(_val2, ListFactory.create())
        val pair1: IPair = ListFactory.createPair(_val1, pair2)

        val result = machine!!.evaluate(ListFactory.createPair(func, pair1))

        ValueTraits.equal(result, pair1) shouldBe true
        ValueTraits.eq(result, pair1) shouldBe false
    }

    test("testDefineNormal") {
        machine!!.evaluate(ListFactory.create("define", "a", _val1))

        machine!!.evaluate("a") shouldBeSameInstanceAs _val1
    }

    test("testApplication") {
        evaluate("(define f (lambda (x) x))")

        machine!!.evaluate(
            ListFactory.create("f", _val1)
        ) shouldBeSameInstanceAs _val1
    }

    test("testDefineFunction") {
        evaluate("(define (f x y) x)")
        evaluate("(define (g . x) x)")

        machine!!.evaluate("f").shouldBeInstanceOf<Function>()

        machine!!.evaluate(
            ListFactory.create("f", _val1, _val2)
        ) shouldBeSameInstanceAs _val1
    }

    test("testCallCC") {
        machine!!.evaluate(
            ListFactory.create(
                CallCCFunction, ListFactory.create(
                    "lambda",
                    ListFactory.create("return"),
                    ListFactory.create("return", _val1)
                )
            )
        ) shouldBeSameInstanceAs _val1
    }
})
