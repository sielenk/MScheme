/* The implementation of Scheme's 'quote' and other Literals.
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
import MScheme.machine.Result;
import MScheme.machine.Registers;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public final class Literal
    extends Result
{
    public final static String id
    = "$Id$";


    private final Value _value;

    private Literal(Value value)
    {
        _value = value;
    }

    public static Literal create(Value value)
    {
        return new Literal(value);
    }


    protected Value getValue(Registers state)
    {
        return _value;
    }


    public String toString()
    {
        return "lit:" + _value.toString();
    }
}
