/* The implementation of compiled scheme's 'if'.
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

import mscheme.compiler.Compiler;
import mscheme.compiler.IForceable;
import mscheme.exceptions.CompileError;
import mscheme.machine.IInvokeable;
import mscheme.machine.Registers;
import mscheme.values.ValueTraits;


public final class Selection
    implements IReduceable
{
    public final static String CVS_ID
        = "$Id$";


    private final Object _test;
    private final Object _onTrue;
    private final Object _onFalse;

    protected Selection(
		Object test,
		Object onTrue,
		Object onFalse
    )
    {
        _test    = test;
        _onTrue  = onTrue;
        _onFalse = onFalse;
    }

    public static Object create(
		Object test,
		Object onTrue,
		Object onFalse
    )
    {
        return new ForceableSelection(test, onTrue, onFalse);
    }

    public String toString()
    {
        return "sel:<" + _test + ", " + _onTrue + ", " + _onFalse + '>';
    }

	/* (non-Javadoc)
	 * @see mscheme.code.Reduceable#reduce(mscheme.machine.Stack, mscheme.environment.DynamicEnvironment)
	 */
	public Object reduce(Registers registers)
	{
		registers.push(
			new IInvokeable()
			{
				public Object invoke(
					Registers registers,
					Object    value)
				{
					return
						ValueTraits.isTrue(value)
						? _onTrue
						: _onFalse;
				}				
			});

		return _test;
	}
}

final class ForceableSelection
implements IForceable
{
    public final static String CVS_ID
    	= "$Id$";


    private final Object _test;
    private final Object _onTrue;
    private final Object _onFalse;

    public ForceableSelection(
        Object test,
        Object onTrue,
        Object onFalse
    )
    {
        _test    = test;
        _onTrue  = onTrue;
        _onFalse = onFalse;
    }

    public Object force()
    throws CompileError
    {
        return new Selection(
        	Compiler.force(_test),
        	Compiler.force(_onTrue),
        	Compiler.force(_onFalse));
    }
}
