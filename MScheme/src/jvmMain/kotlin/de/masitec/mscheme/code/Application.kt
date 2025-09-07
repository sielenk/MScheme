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
package de.masitec.mscheme.code

import de.masitec.mscheme.compiler.IForceable
import de.masitec.mscheme.machine.IContinuation
import de.masitec.mscheme.machine.Registers
import de.masitec.mscheme.values.IList
import de.masitec.mscheme.values.ListFactory
import de.masitec.mscheme.values.ValueTraits

class Application private constructor(
    private val _application: Array<Any?>
) : IForceable, IReduceable {
    override fun force(): Application {
        CodeArray.force(_application)
        return this
    }

    override fun toString(): String =
        "app:${CodeArray.printTuple(_application)}"

    override fun reduce(state: Registers): Any? =
        prepareNext(
            state, ListFactory.create(),
            _application.size - 1
        )

    private fun createPush(done: IList, index: Int): IContinuation =
        IContinuation { registers: Registers, value: Any? ->
            prepareNext(
                registers, ListFactory.prepend(value, done),
                index - 1
            )
        }


    private fun prepareNext(
        registers: Registers, done: IList, index: Int
    ): Any? {
        registers.push(
            if (index == 0)
                createCall(done)
            else
                createPush(done, index)
        )

        return _application[index]
    }

    companion object {
        fun create(application: Array<Any?>): Application =
            Application(application)

        fun createCall(done: IList): IContinuation =
            IContinuation { registers: Registers, value: Any? ->
                ValueTraits.apply(registers, value, done)
            }
    }
}
