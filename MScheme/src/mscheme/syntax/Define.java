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

package mscheme.syntax;

import mscheme.Syntax;
import mscheme.code.Forceable;
import mscheme.code.Reduceable;
import mscheme.environment.Reference;
import mscheme.environment.StaticEnvironment;
import mscheme.exceptions.SchemeException;
import mscheme.util.Arity;
import mscheme.values.List;
import mscheme.values.ListFactory;
import mscheme.values.ValueTraits;


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


    protected void preTranslate(StaticEnvironment compilationEnv)
    { }

    protected Forceable checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        if (ValueTraits.isPair(arguments.getHead()))
        {
            //    (define (f x y) (+ x y))
            // -> (define f (lambda (x y) (+ x y)))
			String symbol  = ValueTraits.toSymbol(ValueTraits.toPair(arguments.getHead()).getFirst ());
            Object formals = ValueTraits.toPair(arguments.getHead()).getSecond();
            List   body    = arguments.getTail();

            Reference ref = compilationEnv.define(symbol);
            compilationEnv.setStateDefinitionBody(symbol);
            try
            {
                return Set.translate(
                    ref,
                    Lambda.INSTANCE.translate(
                        compilationEnv,
                        ListFactory.prepend(
                            formals,
                            body
                        )
                    )
                );
            }
            finally
            {
                compilationEnv.setStateOpen(symbol);
            }
        }
        else
        {
            if (!arguments.getTail().getTail().isEmpty())
            {
                arityError(arguments, Arity.exactly(2));
            }

			String symbol = ValueTraits.toSymbol(arguments.getHead());
            Object value  = arguments.getTail().getHead();

            Reference ref = compilationEnv.define(symbol);
            compilationEnv.setStateDefinitionBody(symbol);
            try
            {
                return Set.translate(
                    ref,
					ValueTraits.getForceable(compilationEnv, value)
                );
            }
            finally
            {
                compilationEnv.setStateOpen(symbol);
            }
        }
    }
}
