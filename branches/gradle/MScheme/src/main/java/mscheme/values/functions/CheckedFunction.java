/* A base class for functions with arity checking.
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

import mscheme.exceptions.SchemeException;
import mscheme.machine.Registers;

import mscheme.util.Arity;

import mscheme.values.Function;
import mscheme.values.IList;


public abstract class CheckedFunction
    extends Function
{
    public final static String CVS_ID
        = "$Id$";


    protected abstract Arity getArity();

    protected abstract Object checkedCall(
		mscheme.machine.Registers state,
        IList                      args
    ) throws SchemeException, InterruptedException;


    // implementation of Function

    public final Object call(Registers state, IList arguments)
        throws SchemeException, InterruptedException
    {
        checkArguments(getArity(), arguments);

        return checkedCall(state, arguments);
    }
}
