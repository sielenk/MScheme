/* The implementation of Scheme's 'set!'.
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

import MScheme.environment.Reference;
import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.SymbolNotFoundException;
import MScheme.exceptions.UnexpectedSyntax;


public final class Assignment
    implements Code
{
    public final static String id
    = "$Id$";


    private final Reference _binding;
    private final Code      _valueCalculation;

    private Assignment(
        Reference binding,
        Code      valueCalculation
    )
    {
        _binding          = binding;
        _valueCalculation = valueCalculation;
    }

    public static Assignment create(
        Reference binding,
        Code      valueCalculation
    )
    {
        return new Assignment(binding, valueCalculation);
    }

    public Code executionStep(Registers state)
    {
        new Continuation(state)
        {
            public final static String id
            = "$Id$";


            protected Code execute(Registers regs, Value evaluationResult)
            {
                return 
                    regs
                    .getEnvironment()
                    .assign(_binding, evaluationResult)
                    .getLiteral();
            }


            protected String debugString()
            {
                return "assign<" + _binding + ">";
            }
        };

        return _valueCalculation;
    }

    public Code force(StaticEnvironment global)
        throws SymbolNotFoundException, UnexpectedSyntax
    {
        return create(
            _binding,
            _valueCalculation.force(global)
        );
    }

    public String toString()
    {
        return "set:<" + _binding + ' ' + _valueCalculation + '>';
    }
}
