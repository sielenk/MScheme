/* The implementation of compiled scheme's 'if'.
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

import MScheme.Value;
import MScheme.Code;

import MScheme.machine.Registers;
import MScheme.machine.Continuation;

import MScheme.exceptions.SymbolNotFoundException;
import MScheme.exceptions.UnexpectedSyntax;


public final class Selection
    implements Code
{
    public final static String id
        = "$Id$";


    private final Code _test;
    private final Code _onTrue;
    private final Code _onFalse;

    private Selection(
        Code test,
        Code onTrue,
        Code onFalse
    )
    {
        _test    = test;
        _onTrue  = onTrue;
        _onFalse = onFalse;
    }

    public static Selection create(
        Code test,
        Code onTrue,
        Code onFalse
    )
    {
        return new Selection(test, onTrue, onFalse);
    }

    public Code executionStep(Registers state)
    {
        new Continuation(state)
        {
            public final static String id
            = "$Id$";


            protected Code execute(
                Registers regs,
                Value     evaluatedTest
            )
            {
                return evaluatedTest.isTrue() ? _onTrue : _onFalse;
            }


            protected String debugString()
            {
                return "select:<" + _onTrue + ", " + _onFalse + ">";
            }
        };

        return _test;
    }

    public Code force()
        throws SymbolNotFoundException, UnexpectedSyntax
    {
        return create(
            _test   .force(),
            _onTrue .force(),
            _onFalse.force()
        );
    }

    public String toString()
    {
        return "sel:<" + _test + ", " + _onTrue + ", " + _onFalse + '>';
    }
}
