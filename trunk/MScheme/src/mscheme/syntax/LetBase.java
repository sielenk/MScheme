/* Basic functions for Scheme's 'let', 'letrec' and 'let*'.
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

import mscheme.exceptions.SchemeException;

import mscheme.util.Arity;

import mscheme.values.Empty;
import mscheme.values.List;
import mscheme.values.ListFactory;
import mscheme.values.ValueTraits;


abstract class LetBase
    extends CheckedSyntax
{
    public final static String CVS_ID
        = "$Id$";


    protected LetBase(Arity arity)
    {
        super(arity);
    }


    protected static List[] splitArguments(List arguments)
        throws SchemeException
    {
        List bindings = ValueTraits.toList(arguments.getHead());
        List body     = arguments.getTail();

        List formals = Empty.create();
        List inits   = Empty.create();

        // parse the initializer list
        while (!bindings.isEmpty())
        {
            List  binding = ValueTraits.toList(bindings.getHead());

            Object formal  = binding.getHead();
            Object init    = binding.getTail().getHead();

            formals  = ListFactory.prepend(formal, formals);
            inits    = ListFactory.prepend(init  , inits  );

            bindings = bindings.getTail();
        }

        // If the closure is anonymous, the order of the
        // arguments is irrelevant, as long as the inits
        // and formals match. But if it can be called by
        // the user the order has to match the definition
        // order. And a named-let-closure can be called ...
        // Since the parsing above reverses the lists,
        // they have to be reversed again here.
        formals = formals.getReversed();
        inits   = inits  .getReversed();

        return 
            new List[]
            {
                formals,
                inits,
                body
            };
    }
}
