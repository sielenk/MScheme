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

package MScheme.environment;

import MScheme.Value;

import MScheme.machine.Registers;
import MScheme.machine.Result;
import MScheme.values.Symbol;

import MScheme.exceptions.RuntimeError;


public final class Reference
            extends Result
{
    public final static String id
    = "$Id$";


    private final Symbol _symbol;
    private final int    _level;
    private final int    _index;

    Reference(Symbol symbol, int level, int index)
    {
        _symbol = symbol;
        _level  = level;
        _index  = index;
    }

    public Symbol getSymbol()
    {
        return _symbol;
    }

    int getLevel ()
    {
        return _level;
    }
    int getIndex ()
    {
        return _index;
    }


    protected Value getValue(Registers state)
    throws RuntimeError
    {
        return state.getEnvironment().lookup(this);
    }


    public String toString()
    {
        return "ptr:<" + _level + ", " + _index + ", " + _symbol + '>';
    }
}
