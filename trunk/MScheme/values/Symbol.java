/* The implementation of Scheme's symbols.
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

package MScheme.values;

import java.io.IOException;
import java.io.Writer;

import MScheme.Code;
import MScheme.Syntax;
import MScheme.Value;

import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.SchemeException;
import MScheme.exceptions.SymbolNotFoundException;
import MScheme.exceptions.UnexpectedSyntax;


public final class Symbol
    extends ValueDefaultImplementations
{
    public final static String id
        = "$Id$";


    private final String _javaString;

    private Symbol(String javaString)
    {
        _javaString = javaString.intern();
    }

    public static Symbol create(String javaString)
    {
        return new Symbol(javaString);
    }

    public static Symbol create(ScmString schemeString)
    {
        return create(schemeString.getJavaString());
    }

    private static int _index = 0;

    public static Symbol createUnique()
    {
        return create("#[" + _index++ + "]");
    }


    public String getJavaString()
    {
        return _javaString;
    }


    // specialisation/implementation of Value

    public boolean isSymbol()
    {
        return true;
    }

    public Symbol toSymbol()
    {
        return this;
    }


    public boolean eq(Value other)
    {
        try
        {
            Symbol otherSymbol = (Symbol)other;

            return getJavaString() == otherSymbol.getJavaString();
        }
        catch (ClassCastException e)
        { }

        return false;
    }


    public void write(Writer destination)
        throws IOException
    {
        destination.write(getJavaString());
    }


    public Code getCompiled(StaticEnvironment env)
        throws SymbolNotFoundException, UnexpectedSyntax
    {
        env.setStateClosed();
        return env.getDelayedReferenceFor(this);
    }

    public Syntax getSyntax(StaticEnvironment env)
        throws SchemeException
    {
        Syntax result = env.getSyntaxFor(this);

        if (result == null)
        {
            result = super.getSyntax(env);
        }

        return result;
    }
}
