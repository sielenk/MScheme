/* The implementation of Scheme's characters.
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

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;


public final class ScmChar
            extends ValueDefaultImplementations
{
    public final static String id
    = "$Id$";


    private char _character;

    private ScmChar(char c)
    {
        _character = c;
    }

    public static ScmChar create(char c)
    {
        return new ScmChar(c);
    }

    public char getJavaChar()
    {
        return _character;
    }

    // specialisation of Value

    public boolean isScmChar()
    {
        return true;
    }

    public ScmChar toScmChar()
    {
        return this;
    }


    public boolean eqv(Value other)
    {
        try
        {
            ScmChar otherCharacter = (ScmChar)other;

            return _character == otherCharacter._character;
        }
        catch (ClassCastException e)
        { }

        return false;
    }

    public void write(Writer destination)
    throws IOException
    {
        destination.write("#\\");
        switch (getJavaChar())
        {
        case ' ':
            destination.write("space");
            break;

        case '\n':
            destination.write("newline");
            break;

        default:
            destination.write(getJavaChar());
            break;
        }
    }

    public void display(Writer destination)
    throws IOException
    {
        destination.write(getJavaChar());
    }
}
