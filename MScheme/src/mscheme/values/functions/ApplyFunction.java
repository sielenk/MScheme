/* The 'apply' function.
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

package mscheme.values.functions;

import mscheme.code.Application;

import mscheme.exceptions.SchemeException;

import mscheme.util.Arity;

import mscheme.values.List;
import mscheme.values.ValueTraits;


public class ApplyFunction
    extends CheckedFunction
{
    public final static String id
        = "$Id$";


    public final static ApplyFunction INSTANCE = new ApplyFunction();


    protected Arity getArity()
    {
        return Arity.atLeast(2);
    }

	protected Object checkedCall(
		mscheme.machine.Registers state,
		List      arguments
	) throws SchemeException
	{
		Object func = arguments.getHead();

		// Since the argument list is newly allocated
		// and mutable, it is permissible to modify it. (*)
		// The modification done looks like this:
		// (f 0 1 (2 3)) is changed to (f 0 1 2 3)

		List toBeModified = arguments;
		for (int i = toBeModified.getLength() - 2; i > 0; i--)
		{
			toBeModified = toBeModified.getTail();
		}

		// Now toBeModified referes the pair containing the
		// last but one argument. In the example it would be
		// (1 (2 3)) which is equal to (1 . ((2 3)))

		ValueTraits.toPair(toBeModified).setSecond(
			ValueTraits.toList(toBeModified.getTail().getHead()).getCopy()
		);

		// and here it would have become
		// (1 . (2 3)) which is equal to (1 2 3)
        
		// the call to getCopy() is necessary to keep the
		// statement marked with (*) above true.
		
		state.push(
			Application.createCall(
				arguments.getTail()));

		return func;
	}
}
