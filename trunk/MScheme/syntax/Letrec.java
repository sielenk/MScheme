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

package MScheme.syntax;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.util.Arity;
import MScheme.code.CodeList;
import MScheme.code.CompiledLambda;
import MScheme.code.Application;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.values.functions.*;
import MScheme.values.*;


// *** letrec ***

final class Letrec
    extends CheckedSyntax
{
    public final static String id
        = "$Id$";


    final static Syntax INSTANCE = new Letrec();

    private Letrec()
    {
        super(Arity.atLeast(2));
    }


    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        // (letrec ((<var> <init>) ...) <body>)

        List bindings = arguments.getHead().toList();
        List body     = arguments.getTail();

        List formals = Empty.create();
        List inits   = Empty.create();

        // split the bindings
        while (!bindings.isEmpty())
        {
            List  binding = bindings.getHead().toList();

            Value formal  = binding.getHead();
            Value init    = binding.getTail().getHead();

            formals  = ListFactory.prepend(formal, formals);
            inits    = ListFactory.prepend(init  , inits  );

            bindings = bindings.getTail();
        }

        StaticEnvironment
        bodyCompilationEnv = compilationEnv.newChild(formals);

        CodeList
        compiledBody = body.getCodeList(bodyCompilationEnv);

        // prepend the initialisations to the body
        while (!formals.isEmpty())
        {
            Symbol formal = formals.getHead().toSymbol();
            Value  init   = inits  .getHead();

            compiledBody = CodeList.prepend(
                               Set.translate(
                                   formal.getReference(bodyCompilationEnv),
                                   init  .getCode     (bodyCompilationEnv)
                               ),
                               compiledBody
                           );

            formals = formals.getTail();
            inits   = inits  .getTail();
        }

        return Application.create(
                   CodeList.create(
                       CompiledLambda.create(
                           Arity.exactly(0),
                           bodyCompilationEnv,
                           compiledBody
                       )
                   )
               );
    }
}
