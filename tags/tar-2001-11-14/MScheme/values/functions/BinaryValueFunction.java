/* A base class for binary functions returning a value.
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

package MScheme.values.functions;

import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;

import MScheme.exceptions.SchemeException;


public abstract class BinaryValueFunction
            extends BinaryFunction
{
    public final static String id
    = "$Id$";


    protected final Code checkedCall(
        Registers state,
        Value     fst,
        Value     snd
    ) throws SchemeException
    {
        return checkedCall(fst, snd).getLiteral();
    }

    protected abstract Value checkedCall(Value fst, Value snd)
    throws SchemeException;
}
