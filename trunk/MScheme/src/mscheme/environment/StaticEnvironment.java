/* Maps symbols to References.
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

import java.io.IOException;
import java.io.Writer;

import java.util.Hashtable;

import mscheme.Syntax;
import mscheme.Value;

import mscheme.exceptions.AlreadyBound;
import mscheme.exceptions.CompileError;
import mscheme.exceptions.SymbolNotFoundException;
import mscheme.exceptions.TypeError;
import mscheme.exceptions.UnexpectedSyntax;

import mscheme.values.List;
import mscheme.values.Symbol;
import mscheme.values.ValueDefaultImplementations;
import mscheme.values.ValueTraits;


public class StaticEnvironment
    extends ValueDefaultImplementations
{
    public final static String id
        = "$Id$";


    // ***********************************************************************

    public void writeOn(Writer destination)
        throws IOException
    {
        destination.write("#[static environment]");
    }

    public StaticEnvironment toStaticEnvironment()
    {
        return this;
    }

    // ***********************************************************************

    private final static int OPEN     = 0;
    private final static int DEF_BODY = 1;
    private final static int CLOSED   = 2;

    private StaticEnvironment _parent;
    private Hashtable         _bindings;
    private int               _level;
    private int               _numberOfReferences;
    private int               _state;

    // *** constructors ******************************************************

    StaticEnvironment()
    {
        this(null);
    }

    StaticEnvironment(StaticEnvironment parent)
    {
        _parent   = parent;
        _bindings = new Hashtable();
        _level    = (parent == null) ? 0 : (parent.getLevel() + 1);
        _numberOfReferences = 0;
        _state              = OPEN;
    }


    StaticEnvironment(StaticEnvironment parent, List symbols)
        throws CompileError, TypeError
    {
        this(parent);

        for (
            List tail = symbols;
            !tail.isEmpty();
            tail = tail.getTail())
        {
            define(ValueTraits.toSymbol(tail.getHead()));
        }
    }

    StaticEnvironment(StaticEnvironment parent, Symbol symbol)
        throws CompileError
    {
        this(parent);
        define(symbol);
    }

    // *** instance access ***************************************************

    int getLevel()
    {
        return _level;
    }

    public int getSize()
    {
        return _numberOfReferences;
    }

    // *** implementation of StaticEnvironment *******************************

    public static StaticEnvironment create()
    {
        return new StaticEnvironment();
    }

    public StaticEnvironment getParent()
    {
        return _parent;
    }

    public StaticEnvironment createChild()
    {
        return new StaticEnvironment(this);
    }

    public StaticEnvironment createChild(List symbols)
        throws CompileError, TypeError
    {
        return new StaticEnvironment(this, symbols);
    }

    public StaticEnvironment createChild(Symbol symbol)
        throws CompileError
    {
        return new StaticEnvironment(this, symbol);
    }

    // *** instance access ***************************************************

    public void setStateOpen(Value v)
        throws CompileError
    {
        switch (_state)
        {
        case OPEN:
            throw new CompileError(
                v,
                "no nested definitions"
            );

        case DEF_BODY:
            _state = OPEN;
            break;

        case CLOSED:
            throw new CompileError(
                v,
                "environment already closed (1)"
            );
        }
    }

    public void setStateDefinitionBody(Value v)
        throws CompileError
    {
        switch (_state)
        {
        case OPEN:
            _state = DEF_BODY;
            break;

        case DEF_BODY:
            throw new CompileError(
                v,
                "no nested definitions"
            );

        case CLOSED:
            throw new CompileError(
                v,
                "environment already closed (2)"
            );
        }
    }

    public void setStateClosed()
    {
        if ((_state == OPEN) && (getLevel() > 0))
        {
            _state = CLOSED;
        }
    }

    public Reference define(Symbol symbol)
        throws CompileError
    {
        if (_state != OPEN)
        {
            throw new CompileError(
                symbol,
                "environment already closed (3)"
            );
        }

        try
        {
            String    key = symbol.getJavaString();
            Reference ref = (Reference)_bindings.get(key);

            // if ref is != null
            // the symbol is already bound in the
            // current frame. This define is in fact
            // a lookup.
            if (ref == null)
            {
                ref = Reference.create(
                          symbol,
                          getLevel(),
                          getSize()
                      );

                _bindings.put(key, ref);
                _numberOfReferences++;

                return ref;
            }
            else if (_level == 0)
            {
                return ref;
            }
        }
        catch (ClassCastException e)
        { }

        throw new AlreadyBound(symbol);
    }


    public void defineSyntax(Symbol symbol, Syntax value)
        throws AlreadyBound
    {
        String  key = symbol.getJavaString();

        {
            Object o = _bindings.get(key);
            if ((o != null) && !(o instanceof Syntax))
            {
                throw new AlreadyBound(symbol);
            }
        }

        _bindings.put(key, value);
    }


    private Object lookupNoThrow(Symbol key)
    {
        for (
            StaticEnvironment current = this;
            current != null;
            current = current._parent
        )
        {
            Object result = current._bindings.get(key.getJavaString());

            if (result != null)
            {
                return result;
            }
        }

        return null;
    }

    private Object delayedLookup(Symbol key)
    {
        Object result = lookupNoThrow(key);

        if ((result == null) || (result instanceof Reference))
        {
            return Reference.create(key, this, _state == DEF_BODY);
        }

        return result;
    }

    private Object lookup(Symbol key)
        throws SymbolNotFoundException
    {
        Object result = lookupNoThrow(key);

        if (result == null)
        {
            throw new SymbolNotFoundException(key);
        }

        return result;
    }

    public Syntax getSyntaxFor(Symbol key)
        throws SymbolNotFoundException
    {
        Object result = lookup(key);

        return 
            (result instanceof Syntax)
            ? (Syntax)result
            : null;
    }

    public Reference getDelayedReferenceFor(Symbol key)
        throws UnexpectedSyntax
    {
        try
        {
            return (Reference)delayedLookup(key);
        }
        catch (ClassCastException e)
        {
            throw new UnexpectedSyntax(key);
        }
    }

    public Reference getReferenceFor(Symbol key, boolean restricted)
        throws CompileError
    {
        try
        {
            Reference result = (Reference)lookup(key);

            if (
                restricted
                && (result.getLevel() == getLevel())
                && (getLevel() > 0) // and again: global is special
            )
            {
                throw new CompileError(key, "may not be used here");
            }

            return result;
        }
        catch (ClassCastException e)
        {
            throw new UnexpectedSyntax(key);
        }
    }

    public Reference getReferenceFor(Symbol key)
        throws CompileError
    {
        return getReferenceFor(key, false);
    }

    public boolean isBound(Symbol key)
    {
        return lookupNoThrow(key) != null;
    }

    // ***********************************************************************
}
