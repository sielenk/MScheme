/* A base class for Scheme functions.
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

package MScheme.values;

import java.io.IOException;
import java.io.Writer;

import MScheme.Code;

import MScheme.exceptions.RuntimeArityError;
import MScheme.exceptions.SchemeException;

import MScheme.machine.Registers;

import MScheme.util.Arity;


public abstract class Function
    extends ValueDefaultImplementations
{
    public final static String id
        = "$Id$";


    // specialisation of Value

    public final boolean isFunction()
    {
        return true;
    }

    public final Function toFunction()
    {
        return this;
    }

    public void write(Writer destination)
        throws IOException
    {
        destination.write("#[procedure]");
    }


    public final static int checkArguments(Arity arity, List arguments)
        throws SchemeException
    {
        int len = arguments.getLength();

        if (!arity.isValid(len))
        {
            throw new RuntimeArityError(arguments, arity);
        }

        return len;
    }


    // abstract function interface

    public abstract Code call(Registers state, List arguments)
        throws SchemeException;
}
