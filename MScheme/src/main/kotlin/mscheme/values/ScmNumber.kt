/* A base class/basic implementation of Scheme's numbers.
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
package mscheme.values

import java.io.IOException
import java.io.Writer
import java.math.BigInteger

class ScmNumber private constructor(
    private val _value: BigInteger
) : IComparable, IOutputable {
    // implementation of Compareable

    override fun eq(other: Any?): Boolean =
        this === other

    override fun eqv(other: Any?): Boolean =
        other is ScmNumber && isEqualTo(other)

    override fun equals(other: Any?): Boolean =
        eqv(other)

    val integer: Int
        // number specific
        get() = _value.toInt()


    fun isLessThan(other: ScmNumber): Boolean =
        _value < other._value

    fun isEqualTo(other: ScmNumber): Boolean =
        _value == other._value


    fun negated(): ScmNumber =
        ScmNumber(_value.negate())

    fun plus(other: ScmNumber): ScmNumber =
        ScmNumber(_value.add(other._value))

    fun minus(other: ScmNumber): ScmNumber =
        ScmNumber(_value.subtract(other._value))

    fun reciprocal(): ScmNumber =
        ScmNumber(BigInteger.valueOf(1).divide(_value))

    fun times(other: ScmNumber): ScmNumber =
        ScmNumber(_value.multiply(other._value))

    fun divide(other: ScmNumber): ScmNumber =
        ScmNumber(_value.divide(other._value))

    override fun outputOn(destination: Writer, doWrite: Boolean) {
        destination.write(_value.toString())
    }

    companion object {
        fun create(v: Int): ScmNumber =
            ScmNumber(BigInteger.valueOf(v.toLong()))

        fun create(v: String): ScmNumber =
            ScmNumber(BigInteger(v))
    }
}
