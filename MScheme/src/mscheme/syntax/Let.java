/* The translation function for Scheme's 'let'.
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

package mscheme.syntax;


import mscheme.code.Application;

import mscheme.environment.StaticEnvironment;

import mscheme.exceptions.SchemeException;

import mscheme.util.Arity;

import mscheme.values.IList;
import mscheme.values.ListFactory;
import mscheme.values.ValueTraits;

import mscheme.values.functions.YCombinator;


// *** let ***

final class Let
    extends LetBase
{
    public final static String CVS_ID
        = "$Id$";


    final static ITranslator INSTANCE = new Let();

    private Let()
    {
        super(Arity.atLeast(2));
    }


    protected Object checkedTranslate(
        StaticEnvironment compilationEnv,
        IList              arguments
    ) throws SchemeException, InterruptedException
    {
        String name;
        if (ValueTraits.isSymbol(arguments.getHead()))
        {
            if (arguments.getLength() < 3)
            {
                arityError(arguments);
            }
            // named let
            // (let <var> ((<var> <init>) ...) <body>)
            name      = ValueTraits.toSymbol(arguments.getHead());
            arguments = arguments.getTail();
        }
        else
        {
            // (let ((<var> <init>) ...) <body>)
            name     = null;
        }

        IList[] formalsInitsBody = splitArguments(arguments);
        IList formals = formalsInitsBody[0];
        IList inits   = formalsInitsBody[1];
        IList body    = formalsInitsBody[2];

        if (name != null)
        {
            // for the named let, the usually anonymous
            // closure gets a name to be recursively callable.
            // to ensure this names uniqueness, it is prepended to
            // the formals list.
            formals = ListFactory.prepend(name, formals);
        }

        Object compiledProc =
            Lambda.INSTANCE.translate(
                compilationEnv,
                ListFactory.prepend(formals, body)
            );

        if (name != null)
        {
            // the "raw" closure of a named-let has one additional
            // argument, which is to be bound to the "curried"
            // closure -- the YCombinator does it's magic ...

            compiledProc = Application.create(
                new Object[]
                {
                    YCombinator.INSTANCE,
                    compiledProc
                }
            );
        }

        Object[] compiledLet = inits.getCompiledArray(compilationEnv, 1);

        compiledLet[0] = compiledProc;

        return Application.create(compiledLet);
    }
}
