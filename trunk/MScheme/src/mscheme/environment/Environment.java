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

package mscheme.environment;

import java.io.IOException;
import java.io.Writer;

import mscheme.exceptions.AlreadyBound;
import mscheme.exceptions.CompileError;
import mscheme.exceptions.RuntimeError;

import mscheme.syntax.SyntaxFactory;

import mscheme.values.Symbol;
import mscheme.values.ValueDefaultImplementations;

import mscheme.values.functions.BuiltinTable;


public final class Environment
    extends ValueDefaultImplementations
{
    public final static String CVS_ID
        = "$Id$";


    // *******************************************************************

    public void writeOn(Writer destination)
        throws IOException
    {
        destination.write("#[environment]");
    }

    public Environment toEnvironment()
    {
        return this;
    }

    // *******************************************************************

    private final  StaticEnvironment _bindings;
    private final DynamicEnvironment _values;

    // *******************************************************************

    private Environment(
        StaticEnvironment  bindings,
        DynamicEnvironment values
    )
    {
        _bindings = bindings;
        _values   = values;
    }

    private static Environment create()
    {
        return new Environment(
             StaticEnvironment.create(),
            DynamicEnvironment.create()
        );
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
                Symbol.create("and"),
                SyntaxFactory.getAndToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("or"),
                SyntaxFactory.getOrToken()
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

    public DynamicEnvironment getDynamic()
    {
        return _values;
    }

    // *** Envrionment access ************************************************

    // *** code access (compiletime) ***

    public Reference define(Symbol key, Object value)
        throws CompileError
    {
        Reference newReference = _bindings.define(key);
        assign(newReference, value);
        return newReference;
    }

    // *** value access (runtime) ***

	public Object assign(Reference key, Object value)
    {
        return _values.assign(key, value);
    }

    public Object assign(Symbol key, Object value)
        throws CompileError
    {
        return assign(_bindings.getReferenceFor(key), value);
    }


    public Object lookupNoThrow(Reference ref)
    {
        return _values.lookupNoThrow(ref);
    }

    public Object lookup(Reference ref)
        throws RuntimeError
    {
        Object result = lookupNoThrow(ref);

        if (result == null)
        {
            throw new RuntimeError(
                ref.getSymbol(),
                "uninitialized variable"
            );
        }

        return result;
    }

    public Object lookup(Symbol key)
        throws CompileError,
               RuntimeError
    {
        return lookup(_bindings.getReferenceFor(key));
    }

    // ***********************************************************************
}
