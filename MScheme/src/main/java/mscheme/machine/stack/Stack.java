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

package mscheme.machine.stack;

import mscheme.exceptions.RuntimeError;
import mscheme.machine.StackFrame;


/**
 * @author Marvin H. Sielenkemper
 */
public class Stack implements IStack {

  public static class Mark {

    public final static String CVS_ID
        = "$Id$";
  }

  public static class Slice {

    public final static String CVS_ID
        = "$Id$";

    final Mark _mark;
    final PlainStack _stack;
    Slice _next;

    Slice(Mark mark, Slice next) {
      _mark = mark;
      _stack = new PlainStack();
      _next = next;
    }

    Slice(Slice slice) {
      _mark = slice._mark;
      _stack = slice._stack.getCopy();
      _next = null;
    }
  }

  private Slice _top;

  public Stack() {
    _top = new Slice(null, null);
  }

  public boolean isEmpty() {
    Slice slice = _top;
    while (slice._next != null && slice._stack.isEmpty()) {
      slice = slice._next;
    }

    return slice._stack.isEmpty();
  }

  public StackFrame pop() {
    while (_top._next != null && _top._stack.isEmpty()) {
      _top = _top._next;
    }

    return _top._stack.pop();
  }

  public void push(StackFrame f) {
    _top._stack.push(f);
  }

  // continuation support
  public Slice getContinuation() {
    Slice top = _top;
    _top = new Slice(null, null);
    reinstate(top);
    return top;
  }

  // subcontinuation support
  public Mark createMark() {
    _top = new Slice(new Mark(), _top);
    return _top._mark;
  }

  public Slice cutSlice(Mark mark) throws RuntimeError {
    final Slice top = _top;

    for (Slice slice = top; slice._next != null; slice = slice._next) {
      if (slice._mark == mark) {
        _top = slice._next;
        slice._next = null;

        return top;
      }
    }

    throw new RuntimeError(mark, "stack mark not found");
  }

  public void reinstate(final Slice slice) {
    final Slice oldTop = _top;

    Slice srcSlice = slice;
    Slice dstSlice = _top = new Slice(srcSlice);
    while (srcSlice._next != null) {
      srcSlice = srcSlice._next;
      dstSlice = dstSlice._next = new Slice(srcSlice);
    }

    if (dstSlice._mark != null) {
      dstSlice._next = oldTop;
    }
  }
}
