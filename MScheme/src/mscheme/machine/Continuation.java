/* A callable continuation.
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
import mscheme.exceptions.SchemeException;
import mscheme.machine.StackList.Slice;
import mscheme.machine.StackList.Mark;
import mscheme.values.functions.UnaryFunction;

/**
 * @author sielenk
 *
 */
public class Continuation
	extends UnaryFunction
{
	public final static String id
		= "$Id$";


	private final Slice _slice;
	private final Mark  _mark;

	public Continuation(StackList stack, Mark mark)
	throws RuntimeError
	{
		_slice = mark.cutSlice(stack);
		_mark  = mark;

		stack.reinstate(_slice);
	}

	protected Object checkedCall(
		Registers state,
		Object    argument)
	throws SchemeException
	{
		_mark.cutSlice(state.getStack());
		state.getStack().reinstate(_slice);
		return argument;
	}
}
