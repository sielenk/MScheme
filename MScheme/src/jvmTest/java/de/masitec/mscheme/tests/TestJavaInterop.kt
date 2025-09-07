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

import de.masitec.mscheme.values.IList
import de.masitec.mscheme.values.PairOrList

class TestJavaInterop(name: String?) : TestSchemeBase(name) {
    fun test_staticField() {
        val machine = machine!!
        val field = javaClass.getField("testFieldStatic")

        machine.environment.define("foo", field)

        testFieldStatic = "something"
        check("(foo)", "something")

        testFieldStatic = "something_else"
        check("(foo)", "something_else")
    }

    fun test_field() {
        val machine = machine!!
        val field = javaClass.getField("testField")
        val environment = machine.environment

        environment.define("bar", this)
        environment.define("foo", field)

        testField = "something"
        check("(foo bar)", "something")

        testField = "something_else"
        check("(foo bar)", "something_else")
    }

    fun test_staticMethod() {
        val machine = machine!!
        val method = javaClass.getMethod("staticMethod")
        val environment = machine.environment

        environment.define("foo", method)

        testFieldStatic = "something"
        check("(foo)", "something")

        testFieldStatic = "something_else"
        check("(foo)", "something_else")
    }

    fun test_method() {
        val machine = machine!!
        val method = javaClass.getMethod("method")
        val environment = machine.environment

        environment.define("bar", this)
        environment.define("foo", method)

        testField = "something"
        check("(foo bar)", "something")

        testField = "something_else"
        check("(foo bar)", "something_else")
    }

    fun test_staticMethodList() {
        val machine = machine!!
        val method = javaClass.getMethod("staticMethodList", IList::class.java)
        val environment = machine.environment

        environment.define("foo", method)

        check("(foo)", "()")
        check("(foo 1 2 3)", "(1 2 3)")
    }

    fun test_methodList() {
        val machine = machine!!
        val method = javaClass.getMethod("methodList", IList::class.java)
        val environment = machine.environment

        environment.define("bar", this)
        environment.define("foo", method)

        check("(foo bar)", "()")
        check("(foo bar 1 2 3)", "(1 2 3)")
    }

    fun test_staticMethodArgs() {
        val machine = machine!!
        val method = javaClass.getMethod(
            "staticMethodArgs", Any::class.java,
            Any::class.java
        )
        val environment = machine.environment

        environment.define("foo", method)

        check("(foo 2 #f)", "(2 . #f)")
        check("(foo '(#t #f) '(1 2 3))", "((#t #f) . (1 2 3))")
    }

    fun test_methodArgs() {
        val machine = machine!!
        val method = javaClass.getMethod("methodArgs", Any::class.java, Any::class.java)
        val environment = machine.environment

        environment.define("bar", this)
        environment.define("foo", method)

        check("(foo bar 1 #t)", "(1 . #t)")
        check("(foo bar '() '())", "(() . ()))")
    }

    @JvmField
    var testField: Any? = null

    fun method(): Any? {
        return testField
    }

    fun methodList(arguments: IList?): Any? {
        return arguments
    }

    fun methodArgs(fst: Any?, snd: Any?): Any {
        return PairOrList.create(fst, snd)
    }

    companion object {
        @JvmField
        var testFieldStatic: Any? = null

        @JvmStatic
        fun staticMethod(): Any? {
            return testFieldStatic
        }

        @JvmStatic
        fun staticMethodList(arguments: IList?): Any? {
            return arguments
        }

        @JvmStatic
        fun staticMethodArgs(fst: Any?, snd: Any?): Any {
            return PairOrList.create(fst, snd)
        }
    }
}
