/* StackList class for MScheme.
   Copyright (C) 2004  Marvin H. Sielenkemper

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
package mscheme.machine.stack

import mscheme.exceptions.RuntimeError
import mscheme.machine.StackFrame

/**
 * @author Marvin H. Sielenkemper
 */
class Stack : IStack {
    class Mark

    class Slice {
        val _mark: Mark?
        internal val _stack: PlainStack
        var _next: Slice?

        internal constructor(mark: Mark?, next: Slice?) {
            _mark = mark
            _stack = PlainStack()
            _next = next
        }

        internal constructor(slice: Slice) {
            _mark = slice._mark
            _stack = slice._stack.copy
            _next = null
        }
    }

    private var _top: Slice

    init {
        _top = Slice(null, null)
    }

    override val isEmpty: Boolean
        get() {
            var slice = _top
            while (slice._next != null && slice._stack.isEmpty) {
                slice = slice._next!!
            }

            return slice._stack.isEmpty
        }

    override fun pop(): StackFrame? {
        while (_top._next != null && _top._stack.isEmpty) {
            _top = _top._next!!
        }

        return _top._stack.pop()
    }

    override fun push(f: StackFrame?) {
        _top._stack.push(f)
    }

    val continuation: Slice
        // continuation support
        get() {
            val top = _top
            _top = Slice(null, null)
            reinstate(top)
            return top
        }

    // subcontinuation support
    fun createMark(): Mark {
        val mark = Mark()
        _top = Slice(mark, _top)
        return mark
    }

    @Throws(RuntimeError::class)
    fun cutSlice(mark: Mark): Slice {
        val top = _top

        var slice = top
        while (slice._next != null) {
            if (slice._mark === mark) {
                _top = slice._next!!
                slice._next = null

                return top
            }
            slice = slice._next!!
        }

        throw RuntimeError(mark, "stack mark not found")
    }

    fun reinstate(slice: Slice) {
        val oldTop = _top

        var srcSlice = slice
        _top = Slice(srcSlice)
        var dstSlice = _top
        while (srcSlice._next != null) {
            srcSlice = srcSlice._next!!
            dstSlice._next = Slice(srcSlice)
            dstSlice = dstSlice._next!!
        }

        if (dstSlice._mark != null) {
            dstSlice._next = oldTop
        }
    }
}
