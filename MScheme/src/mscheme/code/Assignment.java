/* The implementation of Scheme's 'set!'.
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

import mscheme.environment.DelayedReference;
import mscheme.environment.Reference;

import mscheme.exceptions.CompileError;

import mscheme.machine.Invokeable;
import mscheme.machine.Registers;


final class ForceableAssignment
	implements Forceable
{
	public final static String id
		= "$Id$";


	private DelayedReference _binding;
	private Forceable        _expression;


	public ForceableAssignment(
		DelayedReference binding,
		Forceable        expression)
	{
		_binding    = binding;
		_expression = expression;
	}

    public Reduceable force() throws CompileError
    {
        return new Assignment(
        	_binding.forceRef(),
        	_expression.force());
    }
}


public final class Assignment
    implements Reduceable
{
	public final static String id
        = "$Id$";


    private Reference  _binding;
    private Reduceable _expression;


	Assignment(Reference binding, Reduceable expression)
	{
		_binding    = binding;
		_expression = expression;		
	}

    public static Forceable create(
        DelayedReference binding,
        Forceable        valueCalculation
    )
    {
        return new ForceableAssignment(binding, valueCalculation);
    }

    public String toString()
    {
        return "set:<" + _binding + ' ' + _expression + '>';
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
					return registers.assign(_binding, value);
				}				
			});

		return _expression;
	}
}
