/* The translation function for Scheme's 'lambda'.
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
import MScheme.Value;

import MScheme.code.CompiledLambda;

import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.SchemeException;

import MScheme.util.Arity;

import MScheme.values.Empty;
import MScheme.values.List;
import MScheme.values.ListFactory;
import MScheme.values.Pair;


final class Lambda
    extends CheckedSyntax
{
    public final static String id
    = "$Id$";


    final static Syntax INSTANCE = new Lambda();

    private Lambda()
    {
        super(Arity.atLeast(2));
    }


    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        Value rawFormals = arguments.getHead();
        List  body       = arguments.getTail();

        final List  formals;
        final Arity arity;

        if (rawFormals.isList())
        {
            formals = rawFormals.toList();
            arity   = Arity.exactly(formals.getLength());
        }
        else
        {
            // rawFormals is an improper list.
            // This happens for lambda expressions
            // with optional parameters like
            // (lambda (x y . rest) [...])
            // or
            // (lambda args [...]).
            // The following code transforms the improper
            // or not-at-all list into a proper one and
            // counts the required parameters (two in the
            // first example and none in second).

            Value current = rawFormals;
            int  minArity = 0;
            List  result  = Empty.create();

            while (current.isPair())
            {
                Pair currentPair = current.toPair();

                ++minArity;
                result  = ListFactory.prepend(currentPair.getFirst(), result);
                current = currentPair.getSecond();
            }

            result = ListFactory.prepend(current, result);

            formals = result.getReversed();
            arity   = Arity.atLeast(minArity);
        }

        StaticEnvironment
        bodyCompilationEnv = compilationEnv.newChild(formals);

        return CompiledLambda.create(
                   arity,
                   bodyCompilationEnv,
                   body
               );
    }
}
