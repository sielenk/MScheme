/* The 'eval' function.
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

import MScheme.Value;
import MScheme.Code;

import MScheme.environment.Environment;

import MScheme.machine.Registers;

import MScheme.exceptions.*;


public final class EvalFunction
    extends BinaryFunction
{
    public final static String id
        = "$Id$";


    public final static EvalFunction INSTANCE = new EvalFunction();

    private EvalFunction()
    { }

    protected Code checkedCall(Registers state, Value fst, Value snd)
        throws SchemeException
    {
        Environment newEnv  = snd.toEnvironment();
        Code        newCode = fst.getCode(newEnv.getStatic());

        state.setEnvironment(newEnv);
        return newCode;
    }
}
