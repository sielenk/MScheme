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
import de.masitec.mscheme.machine.Machine
import de.masitec.mscheme.values.ValueTraits

abstract class TestSchemeBase(name: String?) : TestCase(name) {
    var machine: Machine? = null
        private set


    override fun setUp() {
        machine = Machine()
    }

    override fun tearDown() {
        machine = null
    }

    fun quote(expression: String): Any? {
        return Machine.parse(expression)
    }

    fun eval(expression: String): Any? {
        return machine!!.evaluate(expression)
    }

    fun check(`in`: String, out: String) {
        val value = eval(`in`)
        val expected = quote(out)
        val success = ValueTraits.equal(value, expected)

        if (!success) {
            println(
                "*** evaluation of ***\n" +
                        `in` + '\n' +
                        "*** returned ***\n" +
                        value + '\n' +
                        "*** expected was ***\n" +
                        out + '\n' +
                        "*** end ***"
            )
        }

        assertTrue(success)
    }
}
