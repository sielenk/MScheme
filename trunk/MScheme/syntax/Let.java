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

package MScheme.syntax;

import MScheme.Code;
import MScheme.Syntax;

import MScheme.code.Application;

import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.SchemeException;

import MScheme.util.Arity;

import MScheme.values.List;
import MScheme.values.ListFactory;
import MScheme.values.Symbol;

import MScheme.values.functions.YCombinator;


// *** let ***

final class Let
    extends LetBase
{
    public final static String id
        = "$Id$";


    final static Syntax INSTANCE = new Let();

    private Let()
    {
        super(Arity.atLeast(2));
    }


    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        Symbol name;
        if (arguments.getHead().isSymbol())
        {
            if (arguments.getLength() < 3)
            {
                arityError(arguments);
            }
            // named let
            // (let <var> ((<var> <init>) ...) <body>)
            name     = arguments.getHead().toSymbol();
            arguments = arguments.getTail();
        }
        else
        {
            // (let ((<var> <init>) ...) <body>)
            name     = null;
        }

        List[] formalsInitsBody = splitArguments(arguments);
        List formals = formalsInitsBody[0];
        List inits   = formalsInitsBody[1];
        List body    = formalsInitsBody[2];

        if (name != null)
        {
            // for the named let, the usually anonymous
            // closure gets a name to be recursively callable.
            // to ensure this names uniqueness, it is prepended to
            // the formals list.
            formals = ListFactory.prepend(name, formals);
        }

        Code compiledProc =
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
                new Code[]
                {
                    YCombinator.INSTANCE,
                    compiledProc
                }
            );
        }

        Code[] compiledLet = inits.getCompiledArray(compilationEnv, 1);

        compiledLet[0] = compiledProc;

        return Application.create(compiledLet);
    }
}
