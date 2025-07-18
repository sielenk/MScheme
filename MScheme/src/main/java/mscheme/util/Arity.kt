/* A general purpose arity tester.
   Copyright (C) 2001  Marvin H. Sielenkemper

This file is part of MScheme.

MScheme is free software; you can redistribute it and/or modify 
it under the terms of the GNU General Public License as published by 
the Free Software Foundation; either version 2 of the License, 
or (at your option) any later version. 

MScheme is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details. 

You should have received a copy of the GNU General Public License
along with MScheme; see the file COPYING. If not, write to 
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA  02111-1307, USA. */
package mscheme.util

import mscheme.exceptions.RuntimeArityError

class Arity private constructor(val min: Int, val max: Int) {
    @get:Throws(RuntimeArityError::class)
    val oneLess: Arity
        get() {
            var newMin = this.min - 1

            if (newMin < 0) {
                newMin = 0
            }

            if (allowMore()) {
                return atLeast(newMin)
            } else {
                val newMax = this.max - 1

                if (newMax < 0) {
                    throw RuntimeArityError(null, this)
                }

                return inRange(newMin, newMax)
            }
        }


    fun allowMore(): Boolean =
        this.max == -1


    fun isValid(arity: Int): Boolean {
        val gotEnoughArguments = (this.min <= arity)
        val isMaxArityDisabled = (this.max == -1)
        val gotTooManyArguments = !isMaxArityDisabled && (this.max < arity)

        return (gotEnoughArguments && !gotTooManyArguments)
    }


    override fun toString(): String {
        var result = "" + this.min

        if (allowMore()) {
            result += " or more"
        } else if (this.min != this.max) {
            result += " to " + this.max
        }

        return result
    }

    companion object {
        @JvmStatic
        fun exactly(arity: Int): Arity =
            Arity(arity, arity)

        @JvmStatic
        fun atLeast(arity: Int): Arity =
            Arity(arity, -1)

        @JvmStatic
        fun inRange(lo: Int, hi: Int): Arity =
            Arity(lo, hi)
    }
}
