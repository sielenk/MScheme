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

import mscheme.code.Forceable;
import mscheme.environment.StaticEnvironment;

import mscheme.exceptions.CantCompileException;
import mscheme.exceptions.PairExpected;


public final class Empty
    implements List, Outputable
{
    public final static String id
        = "$Id$";


    private final static Empty       INSTANCE = new Empty();
    private final static Forceable[] ARRAY    = new Forceable[0];

    private Empty()
    { }

    public static Empty create()
    {
        return INSTANCE;
    }


    // implementation of List

	public boolean isValid()
	{
		return true;
	}

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

    public Object getHead()
        throws PairExpected
    {
        throw new PairExpected(this);
    }

    public List getTail()
        throws PairExpected
    {
        throw new PairExpected(this);
    }

    public Forceable[] getForceableArray(StaticEnvironment compilationEnv)
    {
        return ARRAY;
    }

    public Forceable[] getForceableArray(StaticEnvironment compilationEnv, int index)
    {
        return new Forceable[index];
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

	public Forceable getForceable(StaticEnvironment compilationEnv)
		throws CantCompileException
	{
		throw new CantCompileException(this);
	}
}
