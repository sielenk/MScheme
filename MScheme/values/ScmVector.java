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

package MScheme.values;

import java.io.IOException;
import java.io.Writer;

import MScheme.Code;
import MScheme.Value;

import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.CantCompileException;
import MScheme.exceptions.ImmutableException;
import MScheme.exceptions.InvalidVectorIndexException;
import MScheme.exceptions.ListExpected;
import MScheme.exceptions.PairExpected;
import MScheme.exceptions.VectorException;


public final class ScmVector
    extends Compound
{
    public final static String id
        = "$Id$";


    private final static ScmVector _empty = new ScmVector(false, 0, null);

    private final Value[] _data;

    private ScmVector(boolean isConst, Value[] data)
    {
        super(isConst);
        _data = data;

        if (isConst)
        {
            for (int i = 0; i < _data.length; ++i)
            {
                _data[i] = _data[i].getConst();
            }
        }
    }

    private ScmVector(boolean isConst, int size, Value fill)
    {
        super(isConst);
        _data = new Value[size];

        if (isConst)
        {
            fill = fill.getConst();
        }

        for (int i = 0; i < _data.length; ++i)
        {
            _data[i] = fill;
        }
    }


    public static ScmVector create()
    {
        return _empty;
    }

    public static ScmVector create(Value[] data)
    {
        return (data.length == 0) ? _empty : new ScmVector(false, data);
    }

    public static ScmVector createConst(Value[] data)
    {
        return (data.length == 0) ? _empty : new ScmVector(true, data);
    }

    public static ScmVector create(int size, Value fill)
    {
        return (size == 0) ? _empty : new ScmVector(false, size, fill);
    }

    public static ScmVector create(List list)
    {
        return createHelper(list, 0);
    }

    private static ScmVector createHelper(List list, int index)
    {
        if (list.isEmpty())
        {
            return create(index, null);
        }
        else
        {
            try
            {
                ScmVector result = createHelper(
                                       list.getTail(),
                                       index + 1
                                   );

                result._data[index] = list.getHead();

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


    public int getLength()
    {
        return _data.length;
    }


    public Value get(int index)
        throws VectorException
    {
        try
        {
            return _data[index];
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            throw new InvalidVectorIndexException(this, index);
        }
    }

    public void set(int index, Value value)
        throws InvalidVectorIndexException, ImmutableException
    {
        modify();

        try
        {
            _data[index] = value;
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            throw new InvalidVectorIndexException(this, index);
        }
    }


    // specialisation of Value

    public boolean isScmVector()
    {
        return true;
    }

    public ScmVector toScmVector()
    {
        return this;
    }

    protected Value getConstCopy()
    {
        return createConst((Value[])_data.clone());
    }

    public boolean equal(Value other)
    {
        try
        {
            ScmVector otherVector = (ScmVector)other;

            if (_data.length == otherVector._data.length)
            {
                for (int i = 0; i < _data.length; i++)
                {
                    if (!_data[i].equal(otherVector._data[i]))
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

    private void put(Writer destination, boolean doDisplay)
        throws IOException
    {
        destination.write("#(");
        for (int i = 0; i < getLength(); i++)
        {
            if (i > 0)
            {
                destination.write(' ');
            }
            if (doDisplay)
            {
                _data[i].display(destination);
            }
            else
            {
                _data[i].write(destination);
            }
        }
        destination.write(')');
    }

    public void write(Writer destination)
        throws IOException
    {
        put(destination, false);
    }

    public void display(Writer destination)
        throws IOException
    {
        put(destination, true);
    }


    public Code getCode(StaticEnvironment e)
        throws CantCompileException
    {
        throw new CantCompileException(this);
    }

    public List getList()
    {
        List result = Empty.create();
        for (int i = getLength() - 1; i >= 0; i--)
        {
            result = ListFactory.prepend(_data[i], result);
        }
        return result;
    }
}
