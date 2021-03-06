/*
 * A partial implementation of Syntax with argument check. Copyright (C) 2001
 * Marvin H. Sielenkemper
 * 
 * This file is part of MScheme.
 * 
 * MScheme is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 * 
 * MScheme is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * MScheme; see the file COPYING. If not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

package mscheme.syntax;

import mscheme.environment.StaticEnvironment;

import mscheme.exceptions.SchemeException;
import mscheme.exceptions.SyntaxArityError;

import mscheme.util.Arity;

import mscheme.values.IList;

abstract class CheckedTranslator
        implements ITranslator
{
    public final static String CVS_ID = "$Id$";

    private final Arity _arity;

    protected CheckedTranslator(Arity arity)
    {
        _arity = arity;
    }

    protected static void arityError(IList arguments, Arity arity)
            throws SyntaxArityError
    {
        throw new SyntaxArityError(arguments, arity);
    }

    protected void arityError(IList arguments)
            throws SyntaxArityError
    {
        throw new SyntaxArityError(arguments, _arity);
    }

    public final Object translate(StaticEnvironment compilationEnv,
            IList arguments)
            throws SchemeException, InterruptedException
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

    protected abstract Object checkedTranslate(
            StaticEnvironment compilationEnv, IList arguments)
            throws SchemeException, InterruptedException;
}