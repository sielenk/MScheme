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

import mscheme.Value;

import mscheme.exceptions.ImmutableException;
import mscheme.exceptions.InvalidStringIndexException;


public final class ScmString
    extends Compound
{
    public final static String id
        = "$Id$";


    private final char[]  _string;


    private ScmString(boolean isConst, int size, char fill)
    {
        super(isConst);
        _string = new char[size];
        for (int i = 0; i < size; ++i)
        {
            _string[i] = fill;
        }
    }

    private ScmString(boolean isConst, String value)
    {
        super(isConst);
        _string = new char[value.length()];
        value.getChars(
            0,
            value.length(),
            _string,
            0
        );
    }

    public static ScmString create(int size, char fill)
    {
        return new ScmString(false, size, fill);
    }

    public static ScmString create(String javaString)
    {
        return new ScmString(false, javaString);
    }

    public static ScmString createConst(String javaString)
    {
        return new ScmString(true, javaString);
    }

    public static ScmString create(Symbol schemeSymbol)
    {
        return createConst(schemeSymbol.getJavaString());
    }


    public String getJavaString()
    {
        return new String(_string);
    }


    // specialisation of Value

    public boolean isScmString()
    {
        return true;
    }

    public ScmString toScmString()
    {
        return this;
    }


    // implementation of Compound

    protected Value getConstCopy()
    {
        return createConst(getJavaString());
    }


    // accessors

    public int getLength()
    {
        return _string.length;
    }

    public void set(int index, char c)
    throws InvalidStringIndexException, ImmutableException
    {
        modify();
        try
        {
            _string[index] = c;
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            throw new InvalidStringIndexException(this, index);
        }
    }

    public char get(int index)
    throws InvalidStringIndexException
    {
        try
        {
            return _string[index];
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            throw new InvalidStringIndexException(this, index);
        }
    }

    public boolean equal(Value other)
    {
        try
        {
            ScmString otherString = (ScmString)other;

            return getJavaString().compareTo(
                       otherString.getJavaString()
                   ) == 0;
        }
        catch (ClassCastException e)
        { }

        return false;
    }

    public void write(Writer destination)
    throws IOException
    {
        String str = getJavaString();

        destination.write('"'); // "
        for (int i = 0; i < str.length(); i++)
        {
            char c = str.charAt(i);
            switch (c)
            {
            case '\n':
                destination.write("\\n");
                break;

            case '"': // "
                destination.write("\\\"");
                break;

            default:
                destination.write(c);
                break;
            }
        }
        destination.write('"'); // "
    }

    public void display(Writer destination)
    throws IOException
    {
        destination.write(getJavaString());
    }
}
