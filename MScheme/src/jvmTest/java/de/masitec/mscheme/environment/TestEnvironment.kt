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
package de.masitec.mscheme.environment

import junit.framework.TestCase
import de.masitec.mscheme.environment.Environment
import de.masitec.mscheme.exceptions.SchemeRuntimeError
import de.masitec.mscheme.exceptions.SymbolNotFoundException
import de.masitec.mscheme.exceptions.UnexpectedSyntax
import de.masitec.mscheme.syntax.TranslatorFactory.beginToken
import de.masitec.mscheme.values.ListFactory
import java.lang.Boolean
import kotlin.Any
import kotlin.String


class TestEnvironment(name: String) : TestCase(name) {
    private var env: Environment? = null
    private var sym1: String? = null
    private var sym2: String? = null
    private var val1: Any? = null
    private var val2: Any? = null

    override fun setUp() {
        env = Environment.getEmpty()

        sym1 = "test1"
        sym2 = "test2"

        val1 = ListFactory.create()
        val2 = Boolean.TRUE
    }

    override fun tearDown() {
        env = null
        sym2 = null
        sym1 = null
        val2 = null
        val1 = null
    }


    fun testTestPattern() {
        assertNotSame("different symbols are equal (==)", sym1, sym2)
        assertFalse("different symbols are equal (equals)", sym1 == sym2)
        assertNotSame("different entities are equal (==)", val1, val2)
        assertFalse("different entities are equals (equals)", val1 == val2)
    }

    fun testNormal() {
        try {
            env!!.lookup(sym1!!)
            fail("env not empty")
        } catch (e: SymbolNotFoundException) {
        }

        try {
            env!!.assign(sym1!!, val1)
            fail("expected SymbolNotFound exception")
        } catch (e: SymbolNotFoundException) {
        }

        env!!.define(sym1!!, val1)

        assertSame("lookup failed", env!!.lookup(sym1!!), val1)

        env!!.assign(sym1!!, val2)

        assertSame("assign failed", env!!.lookup(sym1!!), val2)
    }

    fun testSyntax() {
        val env = StaticEnvironment(null)

        try {
            env.getSyntaxFor(sym1!!)
            fail("expected SymbolNotFoundException")
        } catch (e: SymbolNotFoundException) {
        }

        try {
            env.getSyntaxFor(sym1!!)
            fail("expected SymbolNotFoundException")
        } catch (e: SymbolNotFoundException) {
        }

        val token = beginToken
        env.defineSyntax(sym1!!, token)

        assertSame(env.getSyntaxFor(sym1!!), token)

        try {
            env.getReferenceFor(sym1!!)
            fail("expected UnexpectedSyntax")
        } catch (e: UnexpectedSyntax) {
        }

        val reference = env.define(sym2!!)

        assertSame(env.getReferenceFor(sym2!!), reference)
    }

    fun testExtendedStatic() {
        env!!.static.define(sym1!!)
        env!!.static.define(sym2!!)

        try {
            env!!.lookup(sym1!!)
            fail("expected UninitializedSymbolException")
        } catch (e: SchemeRuntimeError) {
        }

        env!!.assign(sym2!!, val1)

        assertSame(env!!.lookup(sym2!!), val1)
    }

    fun testDynamic() {
    }
}
