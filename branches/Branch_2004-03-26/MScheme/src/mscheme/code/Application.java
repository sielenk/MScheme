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

import mscheme.exceptions.SchemeException;
import mscheme.exceptions.CompileError;

import mscheme.machine.Invokeable;
import mscheme.machine.Registers;

import mscheme.values.Empty;
import mscheme.values.List;
import mscheme.values.ListFactory;
import mscheme.values.ValueTraits;


final class ForceableApplication
	implements Forceable
{
	public final static String id
		= "$Id$";


	private final Forceable[] _application;

	ForceableApplication(Forceable[] application)
	{
		_application = application;
	}

	public Reduceable force()
		throws CompileError
	{
		return new Application(
			CodeArray.force(_application));
	}	
}

public final class Application
    implements Reduceable
{
    public final static String id
        = "$Id$";


    private final Reduceable[] _application;

    Application(Reduceable[] application)
    {
        _application = application;
    }

    public static Forceable create(Forceable[] application)
    {
        return new ForceableApplication(application);
    }

    public String toString()
    {
        return "app:" + CodeArray.printTuple(_application);
    }


	public Object reduce(Registers registers)
	{
		return prepareNext(
			registers,
			Empty.create(),
			_application.length - 1);
	}
	
	public static Invokeable createCall(final List done)
	{
		return new Invokeable()
		{
			public final static String id
				= "$Id$";

			public Object invoke(
				Registers registers,
				Object    value) throws SchemeException
			{
				return ValueTraits.apply(registers, value, done);
			}
		};		
	}

	private Invokeable createPush(
		final List done,
		final int  index)
	{
		return new Invokeable()
		{
			public final static String id
				= "$Id$";
	
			public Object invoke(
				Registers registers,
				Object    value)
			{
				return prepareNext(
					registers,
					ListFactory.prepend(value, done),
					index - 1);
			}
		};
	}

	private Object prepareNext(
		Registers  registers,
		final List done,
		final int  index
	)
	{
		registers.push(
			(index == 0)
			?   createCall(done)
			:   createPush(done, index));

		return _application[index];
	}
}
