/* The empty list singleton.
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
import MScheme.Value;

import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.CantCompileException;
import MScheme.exceptions.PairExpected;


public final class Empty
    extends ValueDefaultImplementations
    implements List
{
    public final static String id
        = "$Id$";


    private final static Empty  INSTANCE = new Empty();
    private final static Code[] ARRAY    = new Code[0];

    private Empty()
    { }

    public static Empty create()
    {
        return INSTANCE;
    }


    // specialisation of ValueImplementation

    public void write(Writer destination)
        throws IOException
    {
        destination.write("()");
    }

    public boolean isList()
    {
        return true;
    }

    public List toList()
    {
        return this;
    }


    // implementation of List

    public boolean isEmpty()
    {
        return true;
    }

    public List getCopy()
    {
        return this;
    }

    public int getLength()
    {
        return 0;
    }

    public List getReversed()
    {
        return this;
    }

    public Value getHead()
        throws PairExpected
    {
        throw new PairExpected(this);
    }

    public List getTail()
        throws PairExpected
    {
        throw new PairExpected(this);
    }

    public Code getCode(StaticEnvironment compilationEnv)
        throws CantCompileException
    {
        throw new CantCompileException(this);
    }

    public Code[] getCodeArray(StaticEnvironment compilationEnv)
    {
        return ARRAY;
    }

    public Code[] getCodeArray(StaticEnvironment compilationEnv, int index)
    {
        return new Code[index];
    }
}
