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

package MScheme.environment;

import java.io.IOException;
import java.io.Writer;

import java.util.Hashtable;

import MScheme.Syntax;
import MScheme.Value;

import MScheme.exceptions.AlreadyBound;
import MScheme.exceptions.CompileError;
import MScheme.exceptions.DuplicateSymbolException;
import MScheme.exceptions.SymbolNotFoundException;
import MScheme.exceptions.TypeError;
import MScheme.exceptions.UnexpectedSyntax;

import MScheme.syntax.ProcedureCall;

import MScheme.values.List;
import MScheme.values.Symbol;
import MScheme.values.ValueDefaultImplementations;


public class StaticEnvironment
    extends ValueDefaultImplementations
{
    public final static String id
        = "$Id$";


    // ***********************************************************************

    public void write(Writer destination)
        throws IOException
    {
        destination.write("#[static environment]");
    }

    public StaticEnvironment toStaticEnvironment()
    {
        return this;
    }

    // ***********************************************************************

    private StaticEnvironment _parent;
    private Hashtable         _bindings;
    private int               _level;
    private int               _numberOfReferences;

    // *** constructors ******************************************************

    StaticEnvironment()
    {
        this(null);
    }

    StaticEnvironment(StaticEnvironment parent)
    {
        _bindings = new Hashtable();
        _parent   = parent;
        _level    = (parent == null) ? 0 : (parent.getLevel() + 1);
        _numberOfReferences = 0;
    }


    StaticEnvironment(StaticEnvironment parent, List symbols)
        throws CompileError, TypeError
    {
        this(parent);

        int symbolsDefined = 0;

        for (
            List tail = symbols;
            !tail.isEmpty();
            tail = tail.getTail())
        {
            define(tail.getHead().toSymbol());
            symbolsDefined++;
        }

        if (getSize() != symbolsDefined)
        {
            throw new DuplicateSymbolException(symbols);
        }
    }

    StaticEnvironment(StaticEnvironment parent, Symbol symbol)
        throws CompileError
    {
        this(parent);
        define(symbol);
    }

    // *** instance access ***************************************************

    int getLevel ()
    {
        return _level;
    }

    int getSize  ()
    {
        return _numberOfReferences;
    }

    // *** implementation of StaticEnvironment *******************************

    public StaticEnvironment getParent()
    {
        return _parent;
    }

    public StaticEnvironment newChild()
    {
        return new StaticEnvironment(this);
    }

    public StaticEnvironment newChild(List symbols)
        throws CompileError, TypeError
    {
        return new StaticEnvironment(this, symbols);
    }

    public StaticEnvironment newChild(Symbol symbol)
        throws CompileError
    {
        return new StaticEnvironment(this, symbol);
    }

    // *** instance access ***************************************************

    public Reference define(Symbol symbol)
        throws AlreadyBound
    {
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
            }

            return ref;
        }
        catch (ClassCastException e)
        {
            throw new AlreadyBound(symbol);
        }
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
            return Reference.create(key, this);
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

    public Syntax getDelayedSyntaxFor(Symbol key)
        throws SymbolNotFoundException
    {
        Object result = delayedLookup(key);

        return 
            (result instanceof Reference)
            ? ProcedureCall.create((Reference)result)
            : (Syntax)result;
    }

    public Syntax getSyntaxFor(Symbol key)
        throws SymbolNotFoundException
    {
        Object result = lookup(key);

        return 
            (result instanceof Reference)
            ? ProcedureCall.create((Reference)result)
            : (Syntax)result;
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

    public Reference getReferenceFor(Symbol key)
        throws SymbolNotFoundException, UnexpectedSyntax
    {
        try
        {
            return (Reference)lookup(key);
        }
        catch (ClassCastException e)
        {
            throw new UnexpectedSyntax(key);
        }
    }

    public boolean isBound(Symbol key)
    {
        return lookupNoThrow(key) != null;
    }

    // ***********************************************************************
}
