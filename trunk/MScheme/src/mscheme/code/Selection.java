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

import mscheme.Code;

import mscheme.exceptions.CompileError;

import mscheme.machine.Invokeable;
import mscheme.machine.Registers;
import mscheme.values.ValueTraits;


public final class Selection
    implements Forceable, Reduceable
{
    public final static String CVS_ID
        = "$Id$";


    private Object _test;
    private Object _onTrue;
    private Object _onFalse;

    public Selection(
		Object test,
		Object onTrue,
		Object onFalse
    )
    {
        _test    = test;
        _onTrue  = onTrue;
        _onFalse = onFalse;
    }

    public static Selection create(
		Object test,
		Object onTrue,
		Object onFalse
    )
    {
        return new Selection(test, onTrue, onFalse);
    }

    public Object force()
        throws CompileError
    {
        _test    = Code.force(_test);
        _onTrue  = Code.force(_onTrue);
        _onFalse = Code.force(_onFalse);
        return this;
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
			new Invokeable()
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
