/* A partial implementation of Syntax with argument check.
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

package MScheme.syntax;

import MScheme.Code;
import MScheme.Syntax;

import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.SchemeException;
import MScheme.exceptions.SyntaxArityError;

import MScheme.util.Arity;

import MScheme.values.List;


abstract class CheckedSyntax
    implements Syntax
{
    public final static String id
        = "$Id$";

    private final Arity _arity;

    protected CheckedSyntax(Arity arity)
    {
        _arity = arity;
    }

    protected static void arityError(List arguments, Arity arity)
        throws SyntaxArityError
    {
        throw new SyntaxArityError(arguments, arity);
    }

    protected void arityError(List arguments)
        throws SyntaxArityError
    {
        throw new SyntaxArityError(arguments, _arity);
    }

    public final Code translate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        int len = arguments.getLength();

        if (!_arity.isValid(len))
        {
            arityError(arguments);
        }

        preTranslate(compilationEnv);
        return checkedTranslate(compilationEnv, arguments);
    }

    protected void preTranslate(StaticEnvironment compilationEnv)
    {
        compilationEnv.setStateClosed();
    }

    protected abstract Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException;
}
