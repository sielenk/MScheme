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

import de.masitec.mscheme.exceptions.SchemeRuntimeError
import de.masitec.mscheme.exceptions.SymbolNotFoundException
import de.masitec.mscheme.exceptions.UnexpectedSyntax
import de.masitec.mscheme.syntax.TranslatorFactory.beginToken
import de.masitec.mscheme.values.ListFactory
import de.masitec.mscheme.values.ValueTraits
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.shouldBeSameInstanceAs
import io.kotest.matchers.types.shouldNotBeSameInstanceAs


class TestEnvironment : FunSpec({
    var env: Environment? = null
    var sym1: String? = null
    var sym2: String? = null
    var val1: Any? = null
    var val2: Any? = null

    beforeEach {
        env = Environment.getEmpty()

        sym1 = "test1"
        sym2 = "test2"

        val1 = ListFactory.create()
        val2 = ValueTraits.TRUE
    }

    afterEach {
        env = null
        sym2 = null
        sym1 = null
        val2 = null
        val1 = null
    }


    test("testPattern") {
        sym1 shouldNotBeSameInstanceAs sym2
        (sym1 == sym2) shouldBe false
        val1 shouldNotBeSameInstanceAs val2
        (val1 == val2) shouldBe false
    }

    test("normal") {
        shouldThrow<SymbolNotFoundException> {
            env!!.lookup(sym1!!)
        }

        shouldThrow<SymbolNotFoundException> {
            env!!.assign(sym1!!, val1)
        }

        env!!.define(sym1!!, val1)

        env!!.lookup(sym1!!) shouldBeSameInstanceAs val1

        env!!.assign(sym1!!, val2)

        env!!.lookup(sym1!!) shouldBeSameInstanceAs val2
    }

    test("syntax") {
        val env = StaticEnvironment(null)

        shouldThrow<SymbolNotFoundException> {
            env.getSyntaxFor(sym1!!)
        }

        shouldThrow<SymbolNotFoundException> {
            env.getSyntaxFor(sym1!!)
        }

        val token = beginToken
        env.defineSyntax(sym1!!, token)

        env.getSyntaxFor(sym1!!) shouldBeSameInstanceAs token

        shouldThrow<UnexpectedSyntax> {
            env.getReferenceFor(sym1!!)
        }

        val reference = env.define(sym2!!)

        env.getReferenceFor(sym2!!) shouldBeSameInstanceAs reference
    }

    test("extendedStatic") {
        env!!.static.define(sym1!!)
        env!!.static.define(sym2!!)

        shouldThrow<SchemeRuntimeError> {
            env!!.lookup(sym1!!)
        }

        env!!.assign(sym2!!, val1)

        env!!.lookup(sym2!!) shouldBeSameInstanceAs val1
    }

    test("dynamic") {
    }
})
