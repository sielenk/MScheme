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

package mscheme.values;

import java.io.IOException;
import java.io.Writer;

import java.math.BigInteger;

import mscheme.Value;


public class ScmNumber
    extends ValueDefaultImplementations
{
    public final static String id
        = "$Id$";


    private BigInteger _value;

    private ScmNumber(BigInteger v)
    {
        _value = v;
    }

    public static ScmNumber create(int v)
    {
        return new ScmNumber(
            BigInteger.valueOf(v)
        );
    }

    public static ScmNumber create(String v)
        throws NumberFormatException
    {
        return new ScmNumber(
            new BigInteger(v)
        );
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
        return _value.intValue();
    }


    public boolean isLessThan(ScmNumber other)
    {
        return _value.compareTo(other._value) < 0;
    }

    public boolean isEqualTo(ScmNumber other)
    {
        return _value.equals(other._value);
    }


    public ScmNumber negated()
    {
        return new ScmNumber(_value.negate());
    }

    public ScmNumber plus(ScmNumber other)
    {
        return new ScmNumber(_value.add(other._value));
    }

    public ScmNumber minus(ScmNumber other)
    {
        return new ScmNumber(_value.subtract(other._value));
    }

    public ScmNumber reciprocal()
    {
        return new ScmNumber(BigInteger.valueOf(1).divide(_value));
    }

    public ScmNumber times(ScmNumber other)
    {
        return new ScmNumber(_value.multiply(other._value));
    }

    public ScmNumber divide(ScmNumber other)
    {
        return new ScmNumber(_value.divide(other._value));
    }
}
