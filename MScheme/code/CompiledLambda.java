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

import MScheme.environment.DynamicEnvironment;

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


    private final Arity _arity;
    private       Code  _compiledBody;

    private CompiledLambda(
        Arity arity,
        Code  compiledBody
    )
    {
        _arity        = arity;
        _compiledBody = compiledBody;
    }

    public static CompiledLambda create(
        Arity arity,
        Code  compiledBody
    )
    {
        return new CompiledLambda(arity, compiledBody);
    }

    public static CompiledLambda create(
        Arity  arity,
        Code[] compiledBody
    )
    {
        return create(arity, Sequence.create(compiledBody));
    }

    final class Closure
        extends CheckedFunction
    {
        public final static String id
            = "$Id$";


        private final DynamicEnvironment _enclosingEnvironment;

        public Closure(DynamicEnvironment enclosingEnvironment)
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
            List      arguments
        ) throws ListExpected, PairExpected
        {
            DynamicEnvironment newEnvironment =
                _enclosingEnvironment.createChild(
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
        _compiledBody = _compiledBody.force();
        return this;
    }

    public String toString()
    {
        return "lambda:<" + _compiledBody + '>';
    }
}
