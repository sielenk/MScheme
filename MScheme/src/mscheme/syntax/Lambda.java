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

package mscheme.syntax;


import mscheme.code.CompiledLambda;
import mscheme.environment.StaticEnvironment;
import mscheme.exceptions.SchemeException;
import mscheme.util.Arity;
import mscheme.values.IList;
import mscheme.values.ListFactory;
import mscheme.values.IPair;
import mscheme.values.ValueTraits;


final class Lambda
    extends CheckedTranslator
{
    public final static String CVS_ID
        = "$Id$";


    final static ITranslator INSTANCE = new Lambda();

    private Lambda()
    {
        super(Arity.atLeast(2));
    }


    protected Object checkedTranslate(
        StaticEnvironment compilationEnv,
        IList              arguments
    ) throws SchemeException
    {
        Object rawFormals = arguments.getHead();
        IList   body       = arguments.getTail();

        final IList  formals;
        final Arity arity;

        if (ValueTraits.isList(rawFormals))
        {
            formals = ValueTraits.toList(rawFormals);
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

            Object current = rawFormals;
            int    minArity = 0;
            IList   result  = ListFactory.create();

            while (ValueTraits.isPair(current))
            {
                IPair currentPair = ValueTraits.toConstPair(current);

                ++minArity;
                result  = ListFactory.prepend(currentPair.getFirst(), result);
                current = currentPair.getSecond();
            }

            result = ListFactory.prepend(current, result);

            formals = result.getReversed();
            arity   = Arity.atLeast(minArity);
        }

        return CompiledLambda.create(
            arity,
            body,
            compilationEnv.createChild(formals)
        );
    }
}
