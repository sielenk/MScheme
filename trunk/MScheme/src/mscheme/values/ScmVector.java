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

import mscheme.environment.StaticEnvironment;

import mscheme.exceptions.CantCompileException;
import mscheme.exceptions.ImmutableException;
import mscheme.exceptions.InvalidVectorIndexException;
import mscheme.exceptions.PairExpected;
import mscheme.exceptions.VectorException;


public final class ScmVector
    extends Compound
    implements Outputable
{
    public final static String id
        = "$Id$";


    private final static ScmVector _empty = new ScmVector(false, 0, null);

    private final Object[] _data;

    private ScmVector(boolean isConst, Object[] data)
    {
        super(isConst);
        _data = data;

        if (isConst)
        {
            for (int i = 0; i < _data.length; ++i)
            {
                _data[i] = ValueTraits.getConst(_data[i]);
            }
        }
    }

    private ScmVector(boolean isConst, int size, Object fill)
    {
        super(isConst);
        _data = new Object[size];

        if (isConst)
        {
            fill = ValueTraits.getConst(fill);
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

    public static ScmVector create(Object[] data)
    {
        return (data.length == 0) ? _empty : new ScmVector(false, data);
    }

    public static ScmVector createConst(Object[] data)
    {
        return (data.length == 0) ? _empty : new ScmVector(true, data);
    }

    public static ScmVector create(int size, Object fill)
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


    public Object get(int index)
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

    public void set(int index, Object value)
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

    protected Object getConstCopy()
    {
        return createConst((Object[])_data.clone());
    }

    public boolean equal(Object other)
    {
        try
        {
            ScmVector otherVector = (ScmVector)other;

            if (_data.length == otherVector._data.length)
            {
                for (int i = 0; i < _data.length; i++)
                {
                    if (!ValueTraits.equal(_data[i], otherVector._data[i]))
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

    public void outputOn(Writer destination, boolean doDisplay)
        throws IOException
    {
        destination.write("#(");
        for (int i = 0; i < getLength(); i++)
        {
            if (i > 0)
            {
                destination.write(' ');
            }

			ValueTraits.output(destination, doDisplay, _data[i]);
        }
        destination.write(')');
    }


    public Object getCompiled(StaticEnvironment e)
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
