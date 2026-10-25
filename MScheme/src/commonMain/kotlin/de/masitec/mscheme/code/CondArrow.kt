/*
 * Copyright (C) 2026  Marvin H. Sielenkemper
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
import de.masitec.mscheme.compiler.force
import de.masitec.mscheme.machine.Registers
import de.masitec.mscheme.values.ListFactory
import de.masitec.mscheme.values.ValueTraits

/**
 * The `(test => receiver)` clause of `cond`: if the value of the test
 * is true, the receiver is called with it, otherwise evaluation
 * continues with the remaining clauses.
 */
class CondArrow internal constructor(
    private val _test: Any?,
    private val _receiver: Any?,
    private val _onFalse: Any?
) : IReduceable {
    override fun toString(): String =
        "arrow:<$_test, $_receiver, $_onFalse>"

    override fun reduce(state: Registers): Any? {
        state.push { state2, value ->
            if (ValueTraits.isTrue(value)) {
                state2.push(Application.createCall(ListFactory.create(value)))
                _receiver
            } else
                _onFalse
        }

        return _test
    }

    companion object {
        fun create(
            test: Any?,
            receiver: Any?,
            onFalse: Any?
        ): Any =
            ForceableCondArrow(test, receiver, onFalse)
    }
}

internal class ForceableCondArrow(
    private val _test: Any?,
    private val _receiver: Any?,
    private val _onFalse: Any?
) : IForceable {
    override fun force(): CondArrow =
        CondArrow(
            _test.force(),
            _receiver.force(),
            _onFalse.force()
        )
}
