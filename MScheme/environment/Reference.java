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
import MScheme.Code;

import MScheme.machine.Registers;
import MScheme.machine.Result;
import MScheme.values.Symbol;

import MScheme.exceptions.AlreadyBound;
import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.UnexpectedSyntax;
import MScheme.exceptions.SymbolNotFoundException;

final class DelayedReference
    extends Reference
{
    public final static String id
        = "$Id$";


    private final Symbol            _key;
    private final StaticEnvironment _env;


    DelayedReference(Symbol key, StaticEnvironment env)
    {
        super(key, -1, -1);

        _key = key;
        _env = env;
    }

    protected Value getValue(Registers state)
        throws RuntimeError
    {
        throw new RuntimeError(_key, "delayed reference");
    }

    public Code force(StaticEnvironment global)
        throws SymbolNotFoundException, UnexpectedSyntax
    {
        try {
            return _env.getReferenceFor(_key);
        }
        catch (SymbolNotFoundException e1)
        {
            try {
                return global.define(_key);
            }
            catch (AlreadyBound e2)
            {
                throw new RuntimeException(e2.toString());
            }
        }
    }
}

public class Reference
    extends Result
{
    public final static String id
        = "$Id$";


    private final Symbol _symbol;
    private final int    _level;
    private final int    _index;

    protected Reference(Symbol symbol, int level, int index)
    {
        _symbol = symbol;
        _level  = level;
        _index  = index;
    }

    static Reference create(Symbol key, StaticEnvironment env)
    {
        return new DelayedReference(key, env);
    }

    static Reference create(Symbol key, int level, int index)
    {
        return new Reference(key, level, index);
    }

    public Symbol getSymbol()
    {
        return _symbol;
    }

    int getLevel()
    {
        return _level;
    }

    int getIndex()
    {
        return _index;
    }

    protected Value getValue(Registers state)
        throws RuntimeError
    {
        return state.getEnvironment().lookup(this);
    }

    public Code force(StaticEnvironment global)
        throws SymbolNotFoundException, UnexpectedSyntax
    {
        return this;
    }

    public String toString()
    {
        return
            "ptr:<" + _level 
            + ", "  + _index
            + ", "  + _symbol 
            + '>';
    }
}
