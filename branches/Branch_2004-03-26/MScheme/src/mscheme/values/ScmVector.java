/* The implementation of Scheme's vectors.
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

package mscheme.values;

import java.io.IOException;
import java.io.Writer;
import java.util.Arrays;

import mscheme.exceptions.ImmutableException;
import mscheme.exceptions.InvalidVectorIndexException;
import mscheme.exceptions.PairExpected;
import mscheme.exceptions.SchemeException;
import mscheme.exceptions.VectorException;


public final class ScmVector
{
    public final static String id
        = "$Id$";


    private final static Object[] _empty = new Object[0];

    public static Object[] create()
    {
        return _empty;
    }

    public static Object[] create(Object[] data)
    {
        return (data.length == 0) ? _empty : data;
    }

    public static Object[] create(int size, Object fill)
    {
    	if (size == 0)
    	{ 
    		return _empty;
    	}
    	else
    	{
		    Object[] result= new Object[size];
		    Arrays.fill(result, fill);
		    return result;
		}
    }

    public static Object[] create(List list)
    {
        return createHelper(list, 0);
    }

    private static Object[] createHelper(List list, int index)
    {
        if (list.isEmpty())
        {
            return create(index, null);
        }
        else
        {
            try
            {
                Object[] result = createHelper(
                                       list.getTail(),
                                       index + 1
                                   );

                result[index] = list.getHead();

                return result;
            }
            catch (PairExpected e)
            {
                throw new RuntimeException(
                    "unexpected PairExpected"
                );
            }
        }
    }

	public static Object copy(Object[] objects)
	{
		if (objects.length == 0)
		{
			return _empty;
		}
		else
		{
			return (Object[])objects.clone();
		}
	}


    public static int getLength(Object[] data)
    {
        return data.length;
    }


    public static Object get(Object[] data, int index)
        throws VectorException
    {
        try
        {
            return data[index];
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            throw new InvalidVectorIndexException(data, index);
        }
    }

    public static void set(Object[] data, int index, Object value)
        throws InvalidVectorIndexException, ImmutableException
    {
        try
        {
            data[index] = value;
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            throw new InvalidVectorIndexException(data, index);
        }
    }


    public static boolean equals(Object[] data, Object other)
    {
        try
        {
            Object[] otherVector = (Object[])other;

            if (data.length == otherVector.length)
            {
                for (int i = 0; i < data.length; i++)
                {
                    if (!ValueTraits.equal(data[i], otherVector[i]))
                    {
                        return false;
                    }
                }

                return true;
            }
        }
        catch (ClassCastException e)
        { }

        return false;
    }

    public static void outputOn(
    	Object[] data,
    	Writer destination,
    	boolean doWrite)
    throws IOException, SchemeException
    {
        destination.write("#(");
        for (int i = 0; i < getLength(data); i++)
        {
            if (i > 0)
            {
                destination.write(' ');
            }

			ValueTraits.output(data[i], destination, doWrite);
        }
        destination.write(')');
    }


    public static List getList(Object[] data)
    {
        List result = Empty.create();
        for (int i = getLength(data) - 1; i >= 0; i--)
        {
            result = ListFactory.prepend(data[i], result);
        }
        return result;
    }
}
