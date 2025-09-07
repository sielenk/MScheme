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
import de.masitec.mscheme.compiler.Compiler
import de.masitec.mscheme.environment.Environment
import de.masitec.mscheme.environment.Environment.Companion.getNullEnvironment
import de.masitec.mscheme.exceptions.*
import de.masitec.mscheme.machine.Machine
import de.masitec.mscheme.values.*
import de.masitec.mscheme.values.Function
import de.masitec.mscheme.values.ListFactory.create
import de.masitec.mscheme.values.ListFactory.createPair
import de.masitec.mscheme.values.ListFactory.prepend
import de.masitec.mscheme.values.ValueTraits.eq
import de.masitec.mscheme.values.ValueTraits.equal
import de.masitec.mscheme.values.functions.CallCCFunction
import java.io.StringReader
import java.lang.Boolean
import kotlin.Any
import kotlin.String

class TestMachine
    (name: String?) : TestCase(name) {
    private var machine: Machine? = null

    private var _sym1: String? = null

    private var _sym2: String? = null

    private var _val1: Any? = null

    private var _val2: Any? = null

    private var _unval: Any? = null

    private var _environment: Environment? = null

    override fun setUp() {
        _sym1 = "test1"
        _sym2 = "test2"

        _val1 = Boolean.TRUE
        _val2 = Boolean.FALSE
        _unval = ListFactory.create()

        _environment = getNullEnvironment()

        machine = Machine(_environment!!)
    }

    override fun tearDown() {
        machine = null
        _environment = null
    }

    private fun evaluate(expression: String): Any? {
        return machine!!.evaluate(
            InputPort.create(StringReader(expression)).read()
        )
    }

    fun testTestValues() {
        val compiler = Compiler(_environment!!.static)

        try {
            compiler.compile(_unval)
            fail("expected CantCompileException")
        } catch (e: CantCompileException) {
        }

        assertNotNull(compiler.compile(_val1))
        assertNotNull(compiler.compile(_val2))
    }

    fun testEnvironment() {
        assertSame(_environment, machine!!.environment)
    }

    fun testValue() {
        assertSame(_val1, machine!!.evaluate(_val1))
        assertSame(_val2, machine!!.evaluate(_val2))
    }

    fun testUnevaluatable() {
        try {
            machine!!.evaluate(_unval)
            fail("evaluated Unevaluatable")
        } catch (e: CantCompileException) {
        }
    }

    private fun define(s: String, v: Any?) {
        _environment!!.define(s, v)
    }

    fun testSymbol() {
        try {
            machine!!.evaluate(_sym1!!)
            fail("expected SymbolNotFoundException")
        } catch (e: SchemeException) {
        }

        define(_sym1!!, _val1)
        define(_sym2!!, _unval)

        assertSame(
            "evaluation to Value failed - ", _val1, machine!!
                .evaluate(_sym1!!)
        )
        assertSame(
            "evaluation to Unevaluatable failed - ", _unval, machine!!
                .evaluate(_sym2!!)
        )
    }

    fun testPair1() {
        try {
            machine!!.evaluate(createPair(_val1, _val2))
            fail("expected ListExpected")
        } catch (e: ListExpected) {
        }
    }

    fun testPair2() {
        try {
            machine!!.evaluate(create(_val1))
            fail("expected FunctionExpected")
        } catch (e: FunctionExpected) {
        }
    }

    fun testQuote() {
        assertSame(
            _unval,
            machine!!.evaluate(
                ListFactory.create(
                    "quote",
                    _unval
                )
            )
        )
    }

    fun testIf() {
        define(_sym1!!, _val1)
        define(_sym2!!, _val2)

        assertSame(
            _val1, machine!!.evaluate(
                prepend(
                    "if",
                    create(Boolean.TRUE, _sym1, _sym2)
                )
            )
        )

        assertSame(
            _val1, machine!!.evaluate(
                prepend(
                    "if",
                    ListFactory.create(Boolean.TRUE, _sym1)
                )
            )
        )

        assertSame(
            _val2, machine!!
                .evaluate(prepend("if", create(Boolean.FALSE, _sym1, _sym2)))
        )
    }

    fun testBegin() {
        define(_sym2!!, _val2)

        try {
            machine!!.evaluate(
                create(
                    "begin", _sym1,
                    _sym2
                )
            )
            fail("begin failed")
        } catch (e: SchemeException) {
        }

        _environment!!.define(_sym1!!, _val1)

        assertSame(
            _val2, machine!!.evaluate(
                create("begin", _sym1, _sym2)
            )
        )
    }

    fun testLambdaFailures() {
        try {
            evaluate("(lambda () #(1 2 3))")
            fail("expected CantCompileException")
        } catch (e: CantCompileException) {
        }

        try {
            evaluate("(lambda (#t) #t)")
            fail("expected SymbolExpected")
        } catch (e: SymbolExpected) {
        }

        try {
            evaluate("(lambda (x y x) #t)")
            fail("expected CompileError")
        } catch (e: CompileError) {
        }
    }

    fun testLambdaNoArgs() {
        val func = machine!!.evaluate(
            create(
                "lambda",
                ListFactory.create(), _val1
            )
        ) as Function?

        assertSame(_val1, machine!!.evaluate(create(func)))

        try {
            machine!!.evaluate(ListFactory.create(func, _unval))
            fail("expected CantEvaluateException")
        } catch (e: CantCompileException) {
        }

        try {
            machine!!.evaluate(ListFactory.create(func, _val1))
            fail("expected RuntimeArityError")
        } catch (e: RuntimeArityError) {
        }
    }

    fun testLambdaWithSimpleArgs() {
        val func = evaluate("(lambda (x y) x)") as Function?

        assertSame(
            _val1, machine!!.evaluate(
                create(
                    func, _val1,
                    _val2
                )
            )
        )

        try {
            machine!!.evaluate(ListFactory.create(func, _val1))
            fail("expected RuntimeArityError")
        } catch (e: RuntimeArityError) {
        }
    }

    fun testLambdaWithOptionalArgs() {
        val func = evaluate("(lambda (x . y) y)") as Function?

        try {
            machine!!.evaluate(create(func))
            fail("expected RuntimeArityError")
        } catch (e: RuntimeArityError) {
        }

        assertSame(
            ListFactory.create(),
            machine!!.evaluate(
                ListFactory.create(
                    func, _val1
                )
            )
        )

        assertSame(
            _val2, (machine!!.evaluate(
                create(
                    func,
                    _val1, _val2
                )
            ) as IList).head
        )
    }

    fun testLambdaOptionalIsNewList() {
        val func = evaluate("(lambda x x)") as Function?

        val pair2: IPair = createPair(_val2, ListFactory.create())
        val pair1: IPair = createPair(_val1, pair2)

        val result = machine!!.evaluate(createPair(func, pair1))

        assertTrue(equal(result, pair1))
        assertFalse(eq(result, pair1))
    }

    fun testDefineNormal() {
        machine!!.evaluate(create("define", "a", _val1))

        assertSame(_val1, machine!!.evaluate("a"))
    }

    fun testApplication() {
        evaluate("(define f (lambda (x) x))")

        assertSame(_val1, machine!!.evaluate(ListFactory.create("f", _val1)))
    }

    fun testDefineFunction() {
        evaluate("(define (f x y) x)")
        evaluate("(define (g . x) x)")

        assertTrue(
            "function creation failed - ",
            machine!!.evaluate("f") is Function
        )

        assertSame(
            "function application failed - ", _val1, machine!!
                .evaluate(create("f", _val1, _val2))
        )
    }

    fun testCallCC() {
        assertSame(
            _val1, machine!!.evaluate(
                ListFactory.create(
                    CallCCFunction, create(
                        "lambda",
                        ListFactory.create("return"),
                        ListFactory.create("return", _val1)
                    )
                )
            )
        )
    }
}
