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
import mscheme.exceptions.CompileError;

import mscheme.machine.Registers;

import mscheme.values.Symbol;


public abstract class Reference
    implements Forceable, Reduceable
{
    public final static String id
        = "$Id$";


    private final Symbol _symbol;

    protected Reference(Symbol symbol)
    {
        _symbol = symbol;
    }

    static Reference create(
        Symbol            key,
        StaticEnvironment env,
        boolean           restricted
    )
    {
        return new DelayedReference(key, env, restricted);
    }

    static Reference create(Symbol key, int level, int index)
    {
        return new ForcedReference(key, level, index);
    }

    public final Symbol getSymbol()
    {
        return _symbol;
    }

    public final Object reduce(
    	Registers state)
    throws RuntimeError
    {
    	return state.getEnvironment().lookup(this);
    }

    abstract int getLevel();
    abstract int getIndex();

    public abstract Reference forceRef()
        throws CompileError;

    public final Object force()
        throws CompileError
    {
        return forceRef();
    }

    public final String toString()
    {
        return
            "ptr:<" + getLevel()
            + ", "  + getIndex()
            + ", "  + _symbol
            + '>';
    }
}

final class DelayedReference
    extends Reference
{
    public final static String id
        = "$Id$";


    private final StaticEnvironment _env;
    private final boolean           _restricted;


    DelayedReference(
        Symbol            key,
        StaticEnvironment env,
        boolean           restricted
    )
    {
        super(key);
        _env        = env;
        _restricted = restricted;
    }


    int getLevel()
    {
        throw new RuntimeException(
            getSymbol().toString() + " delayed reference"
        );
    }
    
    int getIndex()
    {
        throw new RuntimeException(
            getSymbol().toString() + " delayed reference"
        );
    }


    public Reference forceRef()
        throws CompileError
    {
        return _env.getReferenceFor(getSymbol(), _restricted);
    }
}


final class ForcedReference
    extends Reference
{
    public final static String id
        = "$Id$";


    private final int _level;
    private final int _index;

    protected ForcedReference(Symbol symbol, int level, int index)
    {
        super(symbol);
        _level  = level;
        _index  = index;
    }

    int getLevel()
    {
        return _level;
    }

    int getIndex()
    {
        return _index;
    }

    public Reference forceRef()
    {
        return this;
    }
}
