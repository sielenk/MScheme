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

package MScheme.syntax;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.util.Arity;
import MScheme.code.*;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.values.functions.*;
import MScheme.values.*;


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


    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        // (let* ((<var> <init>) ...) <body>)

        List bindings = arguments.getHead().toList();
        List body     = arguments.getTail();

        if (bindings.isEmpty())
        {
            // special handling because the helper won't
            // create a new environment in this case

            return Application.create(
                       CodeList.create(
                           CompiledLambda.create(
                               Arity.exactly(0),
                               compilationEnv.newChild(),
                               body
                           )
                       )
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
{
    public final static String id
    = "$Id$";


    private final List _body;

    LetStarHelper(List body)
    {
        _body = body;
    }

    Code translate(
        StaticEnvironment outerEnvironment,
        List              bindings
    ) throws SchemeException
    {
        if (bindings.isEmpty())
        {
            return Sequence.create(
                       _body.getCodeList(outerEnvironment)
                   );
        }
        else
        {
            List binding = bindings.getHead().toList();

            Symbol formal  = binding.getHead().toSymbol();
            Value  init    = binding.getTail().getHead();

            StaticEnvironment
            innerEnvironment = outerEnvironment.newChild(formal);

            Code innerBody = translate(
                                 innerEnvironment,
                                 bindings.getTail()
                             );

            Code lambda = CompiledLambda.create(
                              Arity.exactly(1),
                              innerEnvironment,
                              innerBody
                          );

            return Application.create(
                       CodeList.create(
                           lambda,
                           init.getCode(outerEnvironment)
                       )
                   );
        }
    }
}
