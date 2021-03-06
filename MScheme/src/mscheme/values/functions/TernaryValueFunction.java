/* A base class for ternary functions returning a value.
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


public abstract class TernaryValueFunction
    extends TernaryFunction
{
    public final static String CVS_ID
        = "$Id$";


    protected final Object checkedCall(
		mscheme.machine.Registers state,
        Object    fst,
        Object    snd,
        Object    trd
    ) throws SchemeException
    {
        return checkedCall(fst, snd, trd);
    }

    protected abstract Object checkedCall(Object fst, Object snd, Object trd)
        throws SchemeException;
}
