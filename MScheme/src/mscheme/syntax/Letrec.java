/* The translation function for Scheme's 'letrec'.
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
import mscheme.code.Sequence;
import mscheme.code.CompiledLambda;
import mscheme.compiler.Compiler;

import mscheme.environment.StaticEnvironment;

import mscheme.exceptions.SchemeException;

import mscheme.util.Arity;

import mscheme.values.IList;
import mscheme.values.Symbol;
import mscheme.values.ValueTraits;


// *** letrec ***

final class Letrec
    extends LetBase
{
    public final static String CVS_ID
        = "$Id$";


    final static ITranslator INSTANCE = new Letrec();

    private Letrec()
    {
        super(Arity.atLeast(2));
    }


    protected Object checkedTranslate(
        StaticEnvironment compilationEnv,
        IList              arguments
    ) throws SchemeException
    {
        // (letrec ((<var> <init>) ...) <body>)

        IList[] formalsInitsBody = splitArguments(arguments);
        IList formals = formalsInitsBody[0];
        IList inits   = formalsInitsBody[1];
        IList body    = formalsInitsBody[2];

        int numberOfFormals = formals.getLength();

        StaticEnvironment
            bodyCompilationEnv = compilationEnv.createChild(formals);

        Object[] compiledBody = body.getCompiledArray(bodyCompilationEnv);

        Object[] compiledLetrec
            = new Object[numberOfFormals + compiledBody.length];

        // prepend the initialisations to the body
        int index = 0;
        while (!formals.isEmpty())
        {
            Symbol formal = ValueTraits.toSymbol(formals.getHead());
            Object init   = inits  .getHead();

            compiledLetrec[index++]
                = Set.translate(
                      bodyCompilationEnv.getReferenceFor(formal),
					  Compiler.getForceable(bodyCompilationEnv, init)
                  );

            formals = formals.getTail();
            inits   = inits  .getTail();
        }

        System.arraycopy(
            compiledBody,
            0,
            compiledLetrec,
            index,
            compiledBody.length
        );


        return Application.create(
            new Object[]
            {
                CompiledLambda.create( 
                    Arity.exactly(0),
                    bodyCompilationEnv.getSize(),
                    Sequence.create(
                        compiledLetrec
                    )
                )
            }
        );
    }
}
