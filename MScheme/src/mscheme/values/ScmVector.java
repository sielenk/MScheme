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

import mscheme.exceptions.ImmutableException;
import mscheme.exceptions.InvalidVectorIndexException;
import mscheme.exceptions.VectorException;


final class ConstScmVector
	extends ScmVector
{
    public ConstScmVector(int size, Object fill)
    {
        super(size, fill);
    }

    public ConstScmVector(Object[] data)
    {
        super(data);
    }

    protected Object elementInit(Object object)
    {
        return ValueTraits.getConst(object);
    }

    public void modify() throws ImmutableException
    {
        throw new ImmutableException(this);
    }    
}

final class MutableScmVector
	extends ScmVector
	implements IMutable
{
    public MutableScmVector(int size, Object fill)
    {
        super(size, fill);
    }

    public MutableScmVector(Object[] data)
    {
        super(data);
    }

    protected Object elementInit(Object object)
    {
        return object;
    }

    public Object getConst()
    {
        return null;
    }

    public void modify()
    { }    
}


public abstract class ScmVector
    implements IComparable, IOutputable
{
    public final static String CVS_ID
        = "$Id$";


    private final static ScmVector EMPTY = new ConstScmVector(0, null);

    public static ScmVector create()
    {
        return EMPTY;
    }

    public static ScmVector create(Object[] data)
    {
        return (data.length == 0) ? EMPTY : new MutableScmVector(data);
    }

    public static ScmVector createConst(Object[] data)
    {
        return (data.length == 0) ? EMPTY : new ConstScmVector(data);
    }

    public static ScmVector create(int size, Object fill)
    {
        return (size == 0) ? EMPTY : new MutableScmVector(size, fill);
    }

    public static ScmVector create(IList list)
    {
        ScmVector result = create(list.getLength(), null);
	
        int     i = 0;
        Object l = list;
        while (l instanceof IConstPair)
        {
            IConstPair p = (IConstPair)l;

            result._data[i++] = p.getFirst();
            l                 = p.getSecond();
        }

        return result;
    }


    private final Object[] _data;

    protected ScmVector(Object[] data)
    {
        int size = data.length;
        
        _data = new Object[size];

        for (int i = 0; i < size; ++i)
        {
            _data[i] = elementInit(data[i]);
        }
    }

    protected ScmVector(int size, Object fill)
    {
        Object element = elementInit(fill);

        _data = new Object[size];

        for (int i = 0; i < _data.length; ++i)
        {
            _data[i] = element;
        }
    }

    abstract protected Object elementInit(Object object);


    public int getLength()
    {
        return _data.length;
    }


    void validateIndex(int index)
    	throws InvalidVectorIndexException
    {
        if ((index < 0) || (getLength() <= index))
        {
            throw new InvalidVectorIndexException(this, index);
        }
    }

    public Object get(int index)
        throws VectorException
    {
        validateIndex(index);
        return _data[index];
    }

    public void set(int index, Object value)
        throws InvalidVectorIndexException, ImmutableException
    {
        validateIndex(index);
        modify();
        _data[index] = value;
    }

    protected abstract void modify()
    	throws ImmutableException;


    // implementation of Comparable

    public boolean eq(Object other)
    {
        return this == other;
    }

    public boolean eqv(Object other)
    {
        return this == other;
    }

    public boolean equals(Object other)
    {
        if (!(other instanceof ScmVector))
        {
            return false;
        }

        ScmVector otherVector = (ScmVector)other;

        if (getLength() != otherVector.getLength())
        {
            return false;
        }
        
        for (int i = 0; i < getLength(); i++)
        {
            if (!ValueTraits.equal(_data[i], otherVector._data[i]))
            {
                return false;
            }
        }

        return true;
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

    public IList getList()
    {
        IList result = ListFactory.create();

        for (int i = getLength() - 1; i >= 0; --i)
        {
            result = ListFactory.prepend(_data[i], result);
        }

        return result;
    }
}
