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

import mscheme.code.IReduceable;
import mscheme.compiler.IForceable;
import mscheme.exceptions.CompileError;
import mscheme.exceptions.RuntimeError;
import mscheme.machine.Registers;


public abstract class Reference
    implements IForceable, IReduceable
{
    public final static String CVS_ID
        = "$Id$";


    private final String _symbol;

    protected Reference(String symbol)
    {
        _symbol = symbol;
    }

    static Reference create(
        String            key,
        StaticEnvironment env,
        boolean           restricted
    )
    {
        return new DelayedReference(key, env, restricted);
    }

    static Reference create(String key, int level, int index)
    {
        return new ForcedReference(key, level, index);
    }

    public final String getSymbol()
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
    public final static String CVS_ID
        = "$Id$";


    private final StaticEnvironment _env;
    private final boolean           _restricted;


    DelayedReference(
        String            key,
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
            getSymbol() + " delayed reference"
        );
    }
    
    int getIndex()
    {
        throw new RuntimeException(
            getSymbol() + " delayed reference"
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
    public final static String CVS_ID
        = "$Id$";


    private final int _level;
    private final int _index;

    protected ForcedReference(String symbol, int level, int index)
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
