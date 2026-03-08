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

import de.masitec.mscheme.machine.Machine
import de.masitec.mscheme.util.writeToString
import de.masitec.mscheme.values.OutputPort
import de.masitec.mscheme.values.ValueTraits
import io.kotest.assertions.withClue
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

abstract class TestSchemeBase : FunSpec() {
    var machine: Machine? = null
        private set

    init {
        beforeEach {
            machine = Machine()
        }

        afterEach {
            machine = null
        }
    }

    fun quote(expression: String): Any? {
        return Machine.parse(expression)
    }

    fun eval(expression: String): Any? {
        return machine!!.evaluate(expression)
    }

    fun check(input: String, output: String) {
        val value = eval(input)
        val expected = quote(output)
        val success = ValueTraits.equal(value, expected)

        val clue = {
            """
            *** evaluation of ***
              $input
            *** returned ***
              ${
                writeToString {
                    OutputPort.create(it).display(value)
                }
            }
            *** expected was ***
              $output
            *** end ***
""".trimIndent()
        }

        withClue(clue) {
            success shouldBe true
        }
    }
}
