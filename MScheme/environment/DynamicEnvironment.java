/* Maps References to Values.
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

package MScheme.environment;

import java.util.Vector;

import MScheme.Value;

import MScheme.exceptions.ListExpected;
import MScheme.exceptions.PairExpected;
import MScheme.exceptions.RuntimeError;

import MScheme.util.Arity;

import MScheme.values.List;


public final class DynamicEnvironment
{
    public final static String id
        = "$Id$";


    // *******************************************************************

    private final int               _level;
    private final Vector[]          _frames;

    // *******************************************************************

    private DynamicEnvironment(
        int      level,
        Vector[] frames
    )
    {
        _level  = level;
        _frames = frames;
    }

    private static DynamicEnvironment create(
        DynamicEnvironment parent,
        int                size
    )
    {
        int      level  = (parent == null) ? 0 : parent._level + 1;
        Vector[] frames = new Vector[level + 1];

        if (level > 0)
        {
            System.arraycopy(
                parent._frames, 0,
                        frames, 0,
                level
            );
        }

        {
            Vector locations = new Vector();
            locations.setSize(size);
            frames[level] = locations;
        }

        return new DynamicEnvironment(level, frames);
    }

    private static DynamicEnvironment create(
        DynamicEnvironment parent,
        Arity              arity,
        List               values
    ) throws PairExpected, ListExpected
    {
        DynamicEnvironment result = create(
            parent,
            arity.getMin() + (arity.allowMore() ? 1 : 0)
        );

        Vector frame = result._frames[result._level];
        List   rest  = values;

        for (int i = 0; i < arity.getMin(); i++)
        {
            frame.setElementAt(
                rest.getHead(),
                i
            );

            rest = rest.getTail();
        }

        if (arity.allowMore())
        {
            frame.setElementAt(
                rest,
                arity.getMin()
            );
        }

        return result;
    }


    public static DynamicEnvironment create()
    {
        return create(null, 0);
    }

    public DynamicEnvironment createChild(int size)
    {
        return create(this, size);
    }

    public DynamicEnvironment createChild(
        Arity             arity,
        List              values
    ) throws ListExpected, PairExpected
    {
        return create(this, arity, values);
    }


    public DynamicEnvironment getParent()
    {
        return new DynamicEnvironment(_level - 1, _frames);
    }

    // *** Envrionment access ************************************************

    // *** value access (runtime) ***

    public Value assign(Reference ref, Value value)
    {
        Vector frame = _frames[ref.getLevel()];
        int    index =         ref.getIndex() ;

        try
        {
            Value result = (Value)frame.elementAt(index);
            frame.setElementAt(value, index);
            return (result != null) ? result : value;
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            frame.setSize(index + 1);
            frame.setElementAt(value, index);
            return value;
        }
    }

    public Value lookupNoThrow(Reference ref)
    {
        try
        {
            return 
                (Value)
                _frames   [ref.getLevel()]
                .elementAt(ref.getIndex());
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            return null;
        }
    }

    public Value lookup(Reference ref)
        throws RuntimeError
    {
        Value result = lookupNoThrow(ref);

        if (result == null)
        {
            throw new RuntimeError(
                ref.getSymbol(),
                "uninitialized variable"
            );
        }

        return result;
    }

    // ***********************************************************************
}
