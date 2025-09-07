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
package de.masitec.mscheme.values

import de.masitec.mscheme.exceptions.ImmutableException
import de.masitec.mscheme.exceptions.InvalidStringIndexException
import java.io.IOException
import java.io.Writer

internal class ConstScmString(
    private val _string: String
) : ScmString() {
    override val length: Int
        get() = _string.length

    override fun get_(index: Int): Char =
        _string[index]

    override fun set_(index: Int, c: Char) {
        throw ImmutableException(this)
    }

    override val javaString: String
        get() = _string
}

internal class MutableScmString : ScmString, IMutable {
    private val _string: CharArray

    constructor(size: Int, fill: Char) {
        _string = CharArray(size) { fill }
    }

    constructor(javaString: String) {
        _string = javaString.toCharArray()
    }

    override val length: Int
        get() = _string.size

    override fun get_(index: Int): Char =
        _string[index]

    override fun set_(index: Int, c: Char) {
        _string[index] = c
    }

    override fun getConst(): Any =
        ConstScmString(javaString)

    override val javaString: String
        get() = String(_string)
}


abstract class ScmString protected constructor() : IComparable, IOutputable {
    abstract val javaString: String

    // accessors
    private fun validateIndex(index: Int) {
        if ((index < 0) || (this.length <= index)) {
            throw InvalidStringIndexException(this, index)
        }
    }

    fun set(index: Int, c: Char) {
        validateIndex(index)
        set_(index, c)
    }

    fun get(index: Int): Char {
        validateIndex(index)
        return get_(index)
    }

    abstract val length: Int

    protected abstract fun set_(index: Int, c: Char)

    protected abstract fun get_(index: Int): Char

    override fun outputOn(destination: Writer, doWrite: Boolean) {
        if (doWrite) {
            destination.write('"'.code) // "
            for (i in 0..<this.length) {
                val c = get_(i)
                when (c) {
                    '\n' -> destination.write("\\n")
                    '"' -> destination.write("\\\"")
                    '\\' -> destination.write("\\\\")
                    else -> destination.write(c.code)
                }
            }
            destination.write('"'.code) // "
        } else {
            destination.write(
                this.javaString
            )
        }
    }

    override fun eq(other: Any?): Boolean =
        this === other

    override fun eqv(other: Any?): Boolean =
        this === other

    override fun equals(other: Any?): Boolean =
        other is ScmString
                && this.javaString.compareTo(other.javaString) == 0


    companion object {
        fun create(size: Int, fill: Char): ScmString =
            MutableScmString(size, fill)

        fun create(javaString: String): ScmString =
            MutableScmString(javaString)

        fun createConst(javaString: String): ScmString =
            ConstScmString(javaString)
    }
}
