/* The 'dynamic-wind' function.
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
import MScheme.machine.WindContinuation;
import MScheme.Code;
import MScheme.code.Application;
import MScheme.Value;
import MScheme.values.Function;

import MScheme.exceptions.*;


public class DynamicWindFunction
            extends TernaryFunction
{
    public final static String id
        = "$Id$";


    public final static DynamicWindFunction INSTANCE
        = new DynamicWindFunction();


    protected Code checkedCall(
        Registers state,
        Value     fst,
        Value     snd,
        Value     trd
    ) throws RuntimeError, TypeError
    {
        Code before = createCall(fst);
        Code thunk  = createCall(snd);
        Code after  = createCall(trd);

        return WindContinuation.create(state, before, thunk, after);
    }

    private static Code createCall(Value v)
    throws FunctionExpected
    {
        Code[] application = new Code[1];
        
        application[0] = v.toFunction().getLiteral();
        
        return Application.create(application);
    }
}