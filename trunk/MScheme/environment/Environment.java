/* Maps References to Locations/Values.
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

import java.io.Writer;
import java.io.IOException;
import java.util.Vector;

import MScheme.Init;

import MScheme.Value;

import MScheme.values.ValueDefaultImplementations;
import MScheme.values.List;
import MScheme.values.Symbol;

import MScheme.util.Arity;

import MScheme.syntax.SyntaxFactory;

import MScheme.values.functions.BuiltinTable;

import MScheme.exceptions.*;


public final class Environment
    extends ValueDefaultImplementations
{
    public final static String id
        = "$Id$";


    // *******************************************************************

    public void write(Writer destination)
        throws IOException
    {
        destination.write("#[environment]");
    }

    public Environment toEnvironment()
    {
        return this;
    }

    // *******************************************************************

    private final StaticEnvironment _bindings;
    private final Vector[]          _frames;

    // *******************************************************************

    private Environment(
        StaticEnvironment bindings,
        Vector[]          frames
    )
    {
        _bindings = bindings;
        _frames   = frames;
    }
 
    private static Environment create()
    {
        return create(new StaticEnvironment(), null);
    }

    private static Environment create(
        StaticEnvironment bindings,
        Environment       parent
    )
    {
        int      level  = bindings.getLevel();
        Vector[] frames = new Vector[level + 1];

        if (level > 0)
        {
            if (parent._bindings != bindings.getParent())
            {
                throw new RuntimeException(
                   "consistency failure: StaticEnvironment parent"
                );
            }

            System.arraycopy(
                parent._frames, 0,
                frames, 0,
                level
            );
        }

        {
            Vector locations = new Vector();
            locations.setSize(bindings.getSize());
            frames[level] = locations;
        }

        return new Environment(bindings, frames);
    }

    private static Environment create(
        StaticEnvironment  bindings,
        Environment        parent,
        Arity              arity,
        List               values
    ) throws PairExpected, ListExpected
    {
        Environment result = create(bindings, parent);

        Vector locations = result._frames[result._bindings.getLevel()];
        List   rest      = values;

        for (int i = 0; i < arity.getMin(); i++)
        {
            locations.setElementAt(
                rest.getHead(),
                i
            );

            rest = rest.getTail();
        }

        if (arity.allowMore())
        {
            locations.setElementAt(
                rest,
                arity.getMin()
            );
        }

        return result;
    }


    public static Environment getEmpty()
    {
        return create();
    }

    public static Environment getNullEnvironment()
    {
        Environment result = getEmpty();

        try
        {
            StaticEnvironment staticBindings = result.getStatic();

            staticBindings.defineSyntax(
                Symbol.create("quote"),
                SyntaxFactory.getQuoteToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("if"),
                SyntaxFactory.getIfToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("begin"),
                SyntaxFactory.getBeginToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("lambda"),
                SyntaxFactory.getLambdaToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("let"),
                SyntaxFactory.getLetToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("let*"),
                SyntaxFactory.getLetStarToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("letrec"),
                SyntaxFactory.getLetrecToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("define"),
                SyntaxFactory.getDefineToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("set!"),
                SyntaxFactory.getSetToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("define-syntax"),
                SyntaxFactory.getDefineSyntaxToken()
            );
        }
        catch (AlreadyBound e)
        {
            throw new RuntimeException(
                "unexpected AlreadyBound in getNullEnvironment()"
            );
        }

        return result;
    }

    public static Environment getSchemeReportEnvironment()
    {
        Environment result = getNullEnvironment();

        try
        {
            for (int i = 0; i < BuiltinTable.builtins.length; i++)
            {
                result.define(
                    Symbol.create(BuiltinTable.builtins[i].getName()),
                    BuiltinTable.builtins[i].getFunc()
                );
            }
        }
        catch (CompileError e)
        {
            throw new RuntimeException(
                      "unexpected CompileError"
                  );
        }

        return result;
    }


    public StaticEnvironment getStatic()
    {
        return _bindings;
    }

    public Environment getParent()
    {
        return new Environment(_bindings.getParent(), _frames);
    }

    public Environment newChild()
    {
        return newChild(_bindings.newChild());
    }

    public Environment newChild(
        StaticEnvironment newFrame
    )
    {
        return create(newFrame, this);
    }

    public Environment newChild(
        StaticEnvironment newFrame,
        Arity             arity,
        List              values
    ) throws ListExpected, PairExpected
    {
        return create(newFrame, this, arity, values);
    }

    // *** Envrionment access ************************************************

    // *** code access (compiletime) ***

    public Reference define(Symbol key, Value value)
        throws CompileError
    {
        Reference newReference = _bindings.define(key);
        assign(newReference, value);
        return newReference;
    }

    // *** value access (runtime) ***

    public Value assign(Reference key, Value value)
    {
        Vector locations = _frames[key.getLevel()];
        int    index     = key.getIndex();

        try
        {
            Value result = (Value)locations.elementAt(index);
            locations.setElementAt(value, index);
            return (result != null) ? result : value;
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            locations.setSize(index + 1);
            locations.setElementAt(value, index);
            return value;
        }
    }

    public Value assign(Symbol key, Value value)
        throws SymbolNotFoundException, UnexpectedSyntax
    {
        return assign(_bindings.getReferenceFor(key), value);
    }


    public Value lookupNoThrow(Reference ref)
    {
        try
        {
            return 
                (Value)
                _frames   [ref.getLevel()]
                .elementAt(ref.getIndex());
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            return null;
        }
    }

    public Value lookup(Reference ref)
        throws RuntimeError
    {
        Value result = lookupNoThrow(ref);

        if (result == null)
        {
            throw new RuntimeError(
                ref.getSymbol(),
                "uninitialized variable"
            );
        }

        return result;
    }

    public Value lookup(Symbol key)
        throws SymbolNotFoundException,
               UnexpectedSyntax,
               RuntimeError
    {
        return lookup(_bindings.getReferenceFor(key));
    }

    // ***********************************************************************
}
