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

package mscheme.values.functions;

import mscheme.compiler.Compiler;
import mscheme.environment.Environment;
import mscheme.exceptions.SchemeException;
import mscheme.values.ValueTraits;


public final class EvalFunction
    extends BinaryFunction
{
    public final static String CVS_ID
        = "$Id$";


    public final static EvalFunction INSTANCE = new EvalFunction();

    private EvalFunction()
    { }

    protected Object checkedCall(
		mscheme.machine.Registers state,
    	Object fst,
    	Object snd)
    throws SchemeException
    {
        Environment newEnv  = ValueTraits.toEnvironment(snd);
        Object      newCode = new Compiler(newEnv.getStatic()).compile(fst);

        state.setEnvironment(newEnv.getDynamic());
        return newCode;
    }
}
