/* The implementation of Scheme's function call.
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

package mscheme.code;

import mscheme.Code;
import mscheme.Value;

import mscheme.exceptions.SchemeException;

import mscheme.machine.Continuation;
import mscheme.machine.Registers;

import mscheme.values.Function;
import mscheme.values.List;


public final class ApplyContinuation
    extends Continuation
{
    public final static String id
        = "$Id$";


    private final List _arguments;


    private ApplyContinuation(
        Registers state,
        List      arguments
    )
    {
        super(state);
//      setEnvironment(null); // Evlis Tail Recursion [Cli:1998]
        _arguments = arguments;
    }

    public static void create(
        Registers state,
        List      arguments
    )
    {
        new ApplyContinuation(state, arguments);
    }

    protected Code executionStep(Registers state, Value value)
        throws SchemeException
    {
        return value.toFunction().call(
            state,
            _arguments
        );
    }


    protected String debugString()
    {
        return "apply:<" + _arguments + ">";
    }
}