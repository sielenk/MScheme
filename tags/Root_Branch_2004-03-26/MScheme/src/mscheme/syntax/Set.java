/* The translation function for Scheme's 'set!'.
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

import mscheme.code.Assignment;

import mscheme.environment.Reference;
import mscheme.environment.StaticEnvironment;

import mscheme.exceptions.SchemeException;

import mscheme.util.Arity;

import mscheme.values.List;
import mscheme.values.Symbol;
import mscheme.values.ValueTraits;


final class Set
    extends CheckedSyntax
{
    public final static String id
       = "$Id$";


    final static Syntax INSTANCE = new Set();

    private Set()
    {
        super(Arity.exactly(2));
    }

    protected Object checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        Symbol symbol = ValueTraits.toSymbol(arguments.getHead());
        Object value  = arguments.getTail().getHead();

        return translate(
            compilationEnv.getDelayedReferenceFor(symbol),
			ValueTraits.getCompiled(compilationEnv, value)
        );
    }

    static Object translate(
        Reference reference,
        Object    code
    )
    {
        return Assignment.create(
            reference,
            code
        );
    }
}
