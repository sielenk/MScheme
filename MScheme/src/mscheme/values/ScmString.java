/* The implementation of Scheme's strings.
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
import mscheme.exceptions.InvalidStringIndexException;

public final class ScmString
{
    public final static String id =
        "$Id$";

    public static char[] create(int size, char fill)
    {
        char[] result = new char[size];

        Arrays.fill(result, fill);

        return result;
    }

    public static char[] create(String javaString)
    {
        return javaString.toCharArray();
    }

    public static String toString(char[] s)
    {
        return new String(s);
    }

	public static char[] copy(char[] cs)
	{
		return (char[])cs.clone();
	}

    // accessors

    public static int getLength(char[] s)
    {
        return s.length;
    }

    public static void set(char[] s, int index, char c)
        throws InvalidStringIndexException, ImmutableException
    {
        try
        {
            s[index] = c;
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            throw new InvalidStringIndexException(s, index);
        }
    }

    public static char get(char[] s, int index)
        throws InvalidStringIndexException
    {
        try
        {
            return s[index];
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            throw new InvalidStringIndexException(s, index);
        }
    }

    public static boolean equals(char[] s, Object other)
    {
        if (other instanceof char[])
        {
            char[] otherString = (char[])other;

            return toString(s).compareTo(toString(otherString))
                == 0;
        }
        else
        {
            return false;
        }
    }

    public static void outputOn(char[] s, Writer destination, boolean doWrite)
        throws IOException
    {
        final String str = toString(s);

        if (doWrite)
        {
            destination.write('"'); // "
            for (int i = 0; i < str.length(); i++)
            {
                char c = str.charAt(i);
                switch (c)
                {
                    case '\n' :
                        destination.write("\\n");
                        break;

                    case '"' : // "
                        destination.write("\\\"");
                        break;

                    default :
                        destination.write(c);
                        break;
                }
            }
            destination.write('"'); // "
        }
        else
        {
            destination.write(str);
        }
    }
}
