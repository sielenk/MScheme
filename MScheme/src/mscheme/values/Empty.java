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
along with mscheme; see the file COPYING. If not, write to 
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA  02111-1307, USA. */

package mscheme.values;

import java.io.IOException;
import java.io.Writer;

import mscheme.environment.StaticEnvironment;

import mscheme.exceptions.CantCompileException;
import mscheme.exceptions.ListExpected;
import mscheme.exceptions.PairExpected;


public final class Empty
    implements IList, IOutputable, ICompileable
{
    public final static String CVS_ID
        = "$Id$";


    public final static Empty INSTANCE = new Empty();

    private final static Object[] ARRAY    = new Object[0];

    private Empty()
    { }


    // specialisation of ValueImplementation

    public boolean isList()
    {
        return true;
    }

    public IList toList()
    {
        return this;
    }


    // implementation of List

    public boolean isValid()
    {
        return true;
    }

    public IList validate() throws ListExpected
    {
        return this;
    }

    public boolean isEmpty()
    {
        return true;
    }

    public IList getCopy()
    {
        return this;
    }

    public int getLength()
    {
        return 0;
    }

    public IList getReversed()
    {
        return this;
    }

    public Object getHead()
        throws PairExpected
    {
        throw new PairExpected(this);
    }

    public IList getTail()
        throws PairExpected
    {
        throw new PairExpected(this);
    }

    public Object getForceable(StaticEnvironment compilationEnv)
        throws CantCompileException
    {
        throw new CantCompileException(this);
    }

    public Object[] getCompiledArray(StaticEnvironment compilationEnv)
    {
        return ARRAY;
    }

    public Object[] getCompiledArray(StaticEnvironment compilationEnv, int index)
    {
        return new Object[index];
    }
   
	public Object[] getArray()
	{
		return ARRAY;
	}	   

	public Object[] getArray(int index)
	{
		return new Object[index];
	}

    public void outputOn(Writer destination, boolean doWrite)
    throws IOException
    {
		destination.write("()");
    }
}
