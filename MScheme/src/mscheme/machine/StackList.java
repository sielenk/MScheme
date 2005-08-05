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

package mscheme.machine;

import mscheme.exceptions.RuntimeError;


/** 
 * Class invariants:
 * <ul>
 * <li><code>_head != null</code></li>
 * </ul>
 *
 * @author Marvin H. Sielenkemper
 */
class StackList implements IStack
{
	private final Mark  _call_cc_mark;
	private Slice       _head;

	StackList()
	{
        _call_cc_mark = new Mark();
		_head         = new Slice(_call_cc_mark, null);
	}

	public boolean isEmpty()
	{
        Slice slice = _head;
        while (slice._next != null && slice._stack.isEmpty())
            slice = slice._next;

        return slice._stack.isEmpty();
	}

	public StackFrame pop()
	{
        while (_head._next != null && _head._stack.isEmpty())
            _head = _head._next;

        return _head._stack.pop();
	}

	public void push(StackFrame f)
	{
		_head._stack.push(f);
	}


	public static class Mark
	{
		public Slice cutSlice(final StackList l) throws RuntimeError
		{
            final Slice head = l._head;

            if (this == l._call_cc_mark)
            {
                // no search neccessary:
                // this mark always denotes the last Slice

                l._head = new Slice(l._call_cc_mark, null);
                return head;
            }

            // scan for a matching slice
            // leave the loop one before the last slice
            
            for (Slice slice = head; slice._next != null; slice = slice._next)
            {
                if (slice._mark == this)
                {
                    l._head = slice._next;
                    slice._next = null;

                    return head;
                }
			}

            throw new RuntimeError(null, "");
		}
	}

    public static class Slice
	{
		public final static String CVS_ID
		   = "$Id$";

        final Mark       _mark;
		final StackPlain _stack;
        Slice            _next;

        Slice(Mark mark, Slice next)
        {
            _mark  = mark;
            _stack = new StackPlain();
            _next  = next;
        }

        Slice(Slice slice, Slice next)
		{
            _mark  = slice._mark;
            _stack = slice._stack.getCopy();
            _next  = next;
		}
	}

	synchronized
	public Continuation getCurrentContinuation()
	throws RuntimeError
	{
		return new Continuation(this, _call_cc_mark);
	}

	public Mark createMark()
	{
        _head = new Slice(new Mark(), _head);
		return _head._mark;
	}

	public void reinstate(final Slice slice)
	{
        final Slice oldHead = _head;

        Slice srcSlice = slice;
        Slice dstSlice = _head = new Slice(srcSlice, oldHead);
        while (srcSlice._next != null)
        {
            srcSlice = srcSlice._next;
            dstSlice = dstSlice._next = new Slice(srcSlice, oldHead);
        }
	}
}
