/* The translation function for Scheme's 'define'.
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

import MScheme.util.Arity;

import MScheme.Value;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.environment.StaticEnvironment;
import MScheme.environment.Reference;

import MScheme.values.ListFactory;
import MScheme.values.List;
import MScheme.values.Pair;
import MScheme.values.Symbol;

import MScheme.exceptions.*;


final class Define
    extends CheckedSyntax
{
    public final static String id
        = "$Id$";


    final static Syntax INSTANCE = new Define();

    private Define()
    {
        super(Arity.atLeast(2));
    }

    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        if (arguments.getHead().isPair())
        {
            //    (define (f x y) (+ x y))
            // -> (define f (lambda (x y) (+ x y)))
            Symbol symbol  = arguments.getHead().toPair().getFirst ().toSymbol();
            Value  formals = arguments.getHead().toPair().getSecond();
            List   body    = arguments.getTail();

            return Set.translate(
                       compilationEnv.define(symbol),
                       Lambda.INSTANCE.translate(
                           compilationEnv,
                           ListFactory.prepend(
                               formals,
                               body
                           )
                       )
                   );
        }
        else
        {
            compilationEnv.define(arguments.getHead().toSymbol());

            // call the translate instead of checkedTranslate
            // to let Set check the argument count again
            return Set.INSTANCE.translate(
                       compilationEnv,
                       arguments
                   );
        }
    }
}