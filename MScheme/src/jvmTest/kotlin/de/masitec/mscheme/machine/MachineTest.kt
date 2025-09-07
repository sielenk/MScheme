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
/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package de.masitec.mscheme.machine

import de.masitec.mscheme.code.Application
import de.masitec.mscheme.code.Assignment
import de.masitec.mscheme.code.Selection
import de.masitec.mscheme.code.Sequence
import de.masitec.mscheme.environment.Environment
import de.masitec.mscheme.environment.Reference
import de.masitec.mscheme.values.ValueTraits
import de.masitec.mscheme.values.functions.AppendFunction
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.nulls.shouldNotBeNull
import io.kotest.matchers.should
import io.kotest.matchers.shouldBe
import io.kotest.matchers.shouldNot
import io.kotest.matchers.types.beTheSameInstanceAs
import kotlin.Any
import kotlin.arrayOf


/**
 * @author sielenk
 */
class MachineTest : FunSpec() {
    val O1 = Any()
    val O2 = Any()
    val O3 = Any()
    var _environment: Environment? = null
    var _key: Reference? = null
    var _machine: Machine? = null

    val machine: Machine
        get() = _machine.shouldNotBeNull()
    val environment: Environment
        get() = _environment.shouldNotBeNull()
    val key: Reference
        get() = _key.shouldNotBeNull()

    init {
        beforeEach {
            _environment = Environment.getNullEnvironment()
            _key = environment.define(ValueTraits.createUniqueSymbol(), O1)
            _machine = Machine(environment)
        }

        afterEach {
            _machine = null
            _key = null
            _environment = null
        }

        test("testObjects") {
            O1 shouldNot beTheSameInstanceAs(O2)
            O1 shouldNot beTheSameInstanceAs(O3)
            O2 shouldNot beTheSameInstanceAs(O3)

            machine.execute(O1) should beTheSameInstanceAs(O1)
            machine.execute(O2) should beTheSameInstanceAs(O2)
            machine.execute(O3) should beTheSameInstanceAs(O3)
        }

        fun execSelection(test: Any?, onTrue: Any?, onFalse: Any?): Any? =
            machine.evaluate(Selection.create(test, onTrue, onFalse))

        test("Selection") {
            execSelection(ValueTraits.TRUE, O1, O2) should beTheSameInstanceAs(O1)
            execSelection(ValueTraits.FALSE, O1, O2) should beTheSameInstanceAs(O2)
            execSelection(O1, O1, O2) should beTheSameInstanceAs(O1)
            execSelection(O2, O1, O2) should beTheSameInstanceAs(O1)
            execSelection(O3, O1, O2) should beTheSameInstanceAs(O1)
        }

        fun execAssign(
            value: Any?
        ): Any {
            machine.execute(
                Assignment.create(_key!!, value)
            )

            return _environment!!.lookup(_key!!)
        }

        test("Assign") {
            execAssign(O2) should beTheSameInstanceAs(O2)
            execAssign(O3) should beTheSameInstanceAs(O3)
        }

        test("Sequence") {
            val sequence = arrayOf<Any?>(Assignment.create(key, O3), O2)

            environment.lookup(key) should beTheSameInstanceAs(O1)
            machine.execute(Sequence.create(sequence)) should
                    beTheSameInstanceAs(O2)
            environment.lookup(key) should beTheSameInstanceAs(O3)
        }

        test("Conj") {
            val assignment: Any = Assignment.create(key, O2)
            val sequence1 = arrayOf<Any?>(ValueTraits.FALSE, assignment)
            val sequence2 = arrayOf<Any?>(ValueTraits.TRUE, assignment)

            environment.lookup(key) should beTheSameInstanceAs(O1)
            machine.execute(Sequence.createConj(sequence1))
            environment.lookup(key) should beTheSameInstanceAs(O1)
            machine.execute(Sequence.createConj(sequence2))
            environment.lookup(key) should beTheSameInstanceAs(O2)
        }

        test("Disj") {
            val assignment: Any = Assignment.create(key, O2)
            val sequence1 = arrayOf<Any?>(ValueTraits.TRUE, assignment)
            val sequence2 = arrayOf<Any?>(ValueTraits.FALSE, assignment)

            environment.lookup(key) should beTheSameInstanceAs(O1)
            machine.execute(Sequence.createDisj(sequence1))
            environment.lookup(key) should beTheSameInstanceAs(O1)
            machine.execute(Sequence.createDisj(sequence2))
            environment.lookup(key) should beTheSameInstanceAs(O2)
        }

        test("Application") {
            val sequence = arrayOf<Any?>(AppendFunction)

            ValueTraits.isEmpty(
                machine.execute(Application.create(sequence))
            ) shouldBe true
        }
    }
}
