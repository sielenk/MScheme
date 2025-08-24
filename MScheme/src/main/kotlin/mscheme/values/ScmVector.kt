/* The implementation of Scheme's vectors.
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

import mscheme.exceptions.ImmutableException
import mscheme.exceptions.InvalidVectorIndexException
import mscheme.exceptions.VectorException
import mscheme.values.ValueTraits
import java.io.IOException
import java.io.Writer

internal class ConstScmVector : ScmVector {
    constructor(size: Int, fill: Any?) : super(size, fill)

    constructor(data: Array<Any?>) : super(data)

    override fun elementInit(obj: Any?): Any? =
        ValueTraits.getConst(obj)

    override fun modify() {
        throw ImmutableException(this)
    }
}

internal class MutableScmVector : ScmVector, IMutable {
    constructor(size: Int, fill: Any?) : super(size, fill)

    constructor(data: Array<Any?>) : super(data)

    override fun elementInit(obj: Any?): Any? =
        obj

    override fun getConst(): Any? =
        null

    override fun modify() {
    }
}


abstract class ScmVector : IComparable, IOutputable {
    private val _data: Array<Any?>

    protected constructor(data: Array<Any?>) {
        _data = Array(data.size) { i ->
            elementInit(data[i])
        }
    }

    protected constructor(size: Int, fill: Any?) {
        val element = elementInit(fill)

        _data = Array(size) { element }
    }

    protected abstract fun elementInit(obj: Any?): Any?


    val length: Int
        get() = _data.size


    fun validateIndex(index: Int) {
        if ((index < 0) || (this.length <= index)) {
            throw InvalidVectorIndexException(this, index)
        }
    }

    fun get(index: Int): Any? {
        validateIndex(index)
        return _data[index]
    }

    fun set(index: Int, value: Any?) {
        validateIndex(index)
        modify()
        _data[index] = value
    }

    protected abstract fun modify()

    // implementation of Comparable
    override fun eq(other: Any?): Boolean =
        this === other

    override fun eqv(other: Any?): Boolean =
        this === other

    override fun equals(other: Any?): Boolean {
        if (other !is ScmVector) {
            return false
        }

        if (this.length != other.length) {
            return false
        }

        for (i in 0..<this.length) {
            if (!ValueTraits.equal(_data[i], other._data[i])) {
                return false
            }
        }

        return true
    }

    override fun outputOn(destination: Writer, doWrite: Boolean) {
        destination.write("#(")
        for (i in 0..<this.length) {
            if (i > 0) {
                destination.write(' '.code)
            }

            ValueTraits.output(destination, doWrite, _data[i])
        }
        destination.write(')'.code)
    }

    val list: IList
        get() {
            var result = ListFactory.create()

            for (i in this.length - 1 downTo 0) {
                result = ListFactory.prepend(_data[i], result)
            }

            return result
        }

    companion object {
        private val EMPTY: ScmVector =
            ConstScmVector(0, null)

        fun create(): ScmVector =
            EMPTY

        fun create(data: Array<Any?>): ScmVector =
            if (data.isEmpty()) EMPTY else MutableScmVector(data)

        fun createConst(data: Array<Any?>): ScmVector =
            if (data.isEmpty()) EMPTY else ConstScmVector(data)

        fun create(size: Int, fill: Any?): ScmVector =
            if (size == 0) EMPTY else MutableScmVector(size, fill)

        fun create(list: IList): ScmVector {
            val result: ScmVector = create(list.length, null)

            var i = 0
            var l: Any? = list
            while (l is IPair) {
                result._data[i++] = l.first
                l = l.second
            }

            return result
        }
    }
}
