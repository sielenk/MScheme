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
package de.masitec.mscheme.machine.stack

import de.masitec.mscheme.exceptions.SchemeRuntimeError
import de.masitec.mscheme.machine.StackFrame

/**
 * @author Marvin H. Sielenkemper
 */
class Stack : IStack {
    class Mark

    class Slice internal constructor(
        val _mark: Mark?,
        internal val _stack: PlainStack,
        var _next: Slice?
    ) {
        internal constructor() : this(
            _mark = null,
            _stack = PlainStack(),
            _next = null
        )

        internal constructor(mark: Mark, next: Slice) : this(
            _mark = mark,
            _stack = PlainStack(),
            _next = next
        )

        internal constructor(slice: Slice) : this(
            _mark = slice._mark,
            _stack = slice._stack.getCopy(),
            _next = null
        )
    }

    private var _top: Slice =
        Slice()


    override val isEmpty: Boolean
        get() {
            var slice = _top

            while (slice._stack.isEmpty) {
                slice = slice._next ?: return true
            }

            return false
        }

    override fun tryPop(): StackFrame? {
        while (true) {
            val frame = _top._stack.tryPop()

            if (frame != null)
                return frame

            _top = _top._next ?: return null
        }
    }

    override fun push(frame: StackFrame) {
        _top._stack.push(frame)
    }

    fun getContinuation(): Slice {
        val top = _top
        _top = Slice()
        reinstate(top)
        return top
    }

    // subcontinuation support
    fun createMark(): Mark =
        Mark().also { _top = Slice(it, _top) }

    fun cutSlice(mark: Mark): Slice {
        val top = _top
        var slice = top

        while (true) {
            val sliceNext = slice._next
                ?: throw SchemeRuntimeError(mark, "stack mark not found")

            if (slice._mark === mark) {
                _top = sliceNext
                slice._next = null

                return top
            } else {
                slice = sliceNext
            }
        }
    }

    fun reinstate(slice: Slice) {
        val oldTop = _top
        var srcSlice = slice
        var dstSlice = Slice(slice)

        _top = dstSlice

        while (true) {
            val srcNext = srcSlice._next ?: break
            val dstNext = Slice(srcNext)

            dstSlice._next = dstNext

            srcSlice = srcNext
            dstSlice = dstNext
        }

        if (dstSlice._mark != null) {
            dstSlice._next = oldTop
        }
    }
}
