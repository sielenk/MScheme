/* A base class/basic implementation of Scheme's numbers.
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


public class ScmNumber
            extends ValueDefaultImplementations
{
    public final static String id
    = "$Id$";


    private int _value;

    private ScmNumber(int v)
    {
        _value = v;
    }

    public static ScmNumber create(int v)
    {
        return new ScmNumber(v);
    }

    // implementation of Value

    public boolean isScmNumber()
    {
        return true;
    }

    public ScmNumber toScmNumber()
    {
        return this;
    }

    public boolean eqv(Value other)
    {
        try
        {
            return isEqualTo((ScmNumber)other);
        }
        catch (ClassCastException e)
        { }

        return false;
    }

    public void write(Writer destination)
    throws IOException
    {
        destination.write("" + _value);
    }


    // number specific

    public int getInteger()
    {
        return _value;
    }


    public boolean isLessThan(ScmNumber other)
    {
        return _value < other._value;
    }

    public boolean isEqualTo(ScmNumber other)
    {
        return _value == other._value;
    }


    public ScmNumber negated()
    {
        return new ScmNumber(-_value);
    }

    public ScmNumber plus(ScmNumber other)
    {
        return new ScmNumber(_value + other._value);
    }

    public ScmNumber minus(ScmNumber other)
    {
        return new ScmNumber(_value - other._value);
    }

    public ScmNumber times(ScmNumber other)
    {
        return new ScmNumber(_value * other._value);
    }
}
