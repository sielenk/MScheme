/* The implementation of scheme's 'lambda'.
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

package MScheme.code;

import java.io.IOException;
import java.io.Writer;

import MScheme.Code;
import MScheme.Value;

import MScheme.environment.Environment;
import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.ListExpected;
import MScheme.exceptions.PairExpected;
import MScheme.exceptions.SchemeException;
import MScheme.exceptions.SymbolNotFoundException;
import MScheme.exceptions.UnexpectedSyntax;

import MScheme.machine.Registers;
import MScheme.machine.Result;

import MScheme.util.Arity;

import MScheme.values.List;

import MScheme.values.functions.CheckedFunction;


public final class CompiledLambda
    extends Result
{
    public final static String id
    = "$Id$";


    private final Arity             _arity;
    private final StaticEnvironment _compiledFormals;
    private final Code              _compiledBody;

    private CompiledLambda(
        Arity             arity,
        StaticEnvironment compiledFormals,
        Code              compiledBody
    )
    {
        _arity           = arity;
        _compiledFormals = compiledFormals;
        _compiledBody    = compiledBody;
    }

    public static CompiledLambda create(
        Arity             arity,
        StaticEnvironment compiledFormals,
        Code              compiledBody
    )
    {
        return new CompiledLambda(arity, compiledFormals, compiledBody);
    }

    public static CompiledLambda create(
        Arity             arity,
        StaticEnvironment compiledFormals,
        Code[]            compiledBody
    )
    {
        return create(arity, compiledFormals, Sequence.create(compiledBody));
    }

    public static CompiledLambda create(
        Arity             arity,
        StaticEnvironment compiledFormals,
        List              body
    ) throws SchemeException
    {
        return create(arity, compiledFormals, body.getCodeArray(compiledFormals));
    }

    final class Closure
        extends CheckedFunction
    {
        public final static String id
            = "$Id$";


        private final Environment _enclosingEnvironment;

        public Closure(Environment enclosingEnvironment)
        {
            _enclosingEnvironment = enclosingEnvironment;
        }

        public void write(Writer destination)
        throws IOException
        {
            destination.write("#[closure]");
        }

        public Arity getArity()
        {
            return _arity;
        }

        protected Code checkedCall(
            Registers state,
            int       length,
            List      arguments
        ) throws ListExpected, PairExpected
        {
            Environment newEnvironment =
                _enclosingEnvironment.newChild(
                    _compiledFormals,
                    getArity(),
                    arguments
                );

            state.setEnvironment(newEnvironment);

            return _compiledBody;
        }
    }

    protected Value getValue(Registers state)
    {
        return new Closure(state.getEnvironment());
    }

    public Code force()
        throws SymbolNotFoundException, UnexpectedSyntax
    {
        return create(
            _arity,
            _compiledFormals,
            _compiledBody.force()
        );
    }

    public String toString()
    {
        return "lambda:<" + _compiledBody + '>';
    }
}
