/* The translation function for Scheme's 'let*'.
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

import mscheme.code.Application;
import mscheme.code.CompiledLambda;
import mscheme.code.Forceable;
import mscheme.code.Sequence;

import mscheme.environment.StaticEnvironment;

import mscheme.exceptions.SchemeException;

import mscheme.util.Arity;

import mscheme.values.Function;
import mscheme.values.List;
import mscheme.values.ValueTraits;


// *** let* ***

final class LetStar
    extends CheckedSyntax
{
    public final static String id
        = "$Id$";


    final static Syntax INSTANCE = new LetStar();

    private LetStar()
    {
        super(Arity.atLeast(2));
    }


    protected Forceable checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        // (let* ((<var> <init>) ...) <body>)

        List bindings = ValueTraits.toList(arguments.getHead());
        List body     = arguments.getTail();

        if (bindings.isEmpty())
        {
            // special handling because the helper won't
            // create a new environment in this case

            return Application.create(
                new Forceable[]
                {
                    CompiledLambda.create(
                        Arity.exactly(0),
                        body,
                        compilationEnv.createChild()
                    )
                }
            );
        }
        else
        {
            return
                new LetStarHelper(body)
                    .translate(compilationEnv, bindings);
        }
    }
}

final class LetStarHelper
	implements Syntax
{
    public final static String id
    = "$Id$";


    private final List _body;

    LetStarHelper(List body)
    {
        _body = body;
    }

	public Forceable translate(
        StaticEnvironment outerEnvironment,
        List              bindings
    ) throws SchemeException
    {
        if (bindings.isEmpty())
        {
            return Sequence.create(
                _body.getForceableArray(outerEnvironment)
            );
        }
        else
        {
            List binding = ValueTraits.toList(bindings.getHead());

			String formal  = ValueTraits.toSymbol(binding.getHead());
            Object init    = binding.getTail().getHead();

            StaticEnvironment innerEnvironment
                = outerEnvironment.createChild(formal);

            Forceable innerBody = translate(
                                      innerEnvironment,
                                      bindings.getTail());

            Forceable lambda = CompiledLambda.create(
                              Arity.exactly(1),
                              innerEnvironment.getSize(),
                              innerBody
                          );

            return Application.create(
                new Forceable[]
                {
                    lambda,
					ValueTraits.getForceable(outerEnvironment, init)
                }
            );
        }
    }
}
