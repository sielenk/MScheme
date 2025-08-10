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
package mscheme.values.functions

import mscheme.exceptions.RuntimeArityError
import mscheme.exceptions.SchemeRuntimeError
import mscheme.exceptions.TypeError
import mscheme.util.Arity.Companion.atLeast
import mscheme.values.IList
import mscheme.values.ValueTraits.toScmNumber

enum class Order {
    LT, LE, EQ, GE, GT;

    companion object {
        @JvmStatic
        @Throws(SchemeRuntimeError::class, TypeError::class)
        fun check(arguments: IList, mode: Order): Boolean {
            val arity = atLeast(2)
            val len = arguments.length

            if (!arity.isValid(len)) {
                throw RuntimeArityError(arguments, arity)
            }

            var curr = toScmNumber(arguments.head)
            var tail = arguments.tail

            var rising = true
            var strict = true
            var falling = true

            do {
                val next = toScmNumber(tail.head)
                tail = tail.tail

                if (curr.isEqualTo(next)) {
                    strict = false
                } else {
                    if (curr.isLessThan(next)) {
                        falling = false
                    } else {
                        rising = false
                    }

                    if (!rising and !falling) {
                        return false
                    }
                }

                curr = next
            } while (!tail.isEmpty)

            return when (mode) {
                LT -> strict and rising
                LE -> rising
                EQ -> rising and falling
                GE -> falling
                GT -> strict and falling
            }
        }
    }
}
