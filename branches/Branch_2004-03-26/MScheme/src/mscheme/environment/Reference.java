/* A compiled Symbol, allows efficient access to environments.
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

package mscheme.environment;

import mscheme.code.Forceable;
import mscheme.code.Reduceable;
import mscheme.exceptions.RuntimeError;
import mscheme.machine.Registers;


public final class Reference
	implements Forceable, Reduceable
{
    public final static String id
        = "$Id$";


    private final String _symbol;
	private final int    _level;
	private final int    _index;


	private Reference(String symbol, int level, int index)
	{
		_symbol = symbol;
		_level  = level;
		_index  = index;
	}

    static DelayedReference create(
		String            key,
        StaticEnvironment env,
        boolean           restricted
    )
    {
        return new DelayedReference(key, env, restricted);
    }

    static Reference create(String key, int level, int index)
    {
        return new Reference(key, level, index);
    }

    public final String getSymbol()
    {
        return _symbol;
    }

	public final Reduceable force()
	{
		return this;
	}

    public final Object reduce(
    	Registers state)
    throws RuntimeError
    {
    	return state.getEnvironment().lookup(this);
    }

    int getLevel()
    {
    	return _level;
    }

    int getIndex()
    {
    	return _index;
    }

    public final String toString()
    {
        return
            "ptr:<" + _level
            + ", "  + _index
            + ", "  + _symbol
            + '>';
    }
}