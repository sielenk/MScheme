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

package mscheme.environment;

import java.util.Vector;

import mscheme.exceptions.ListExpected;
import mscheme.exceptions.PairExpected;
import mscheme.exceptions.RuntimeError;

import mscheme.util.Arity;

import mscheme.values.IList;


public final class DynamicEnvironment
{
    public final static String CVS_ID
        = "$Id$";


    // *******************************************************************

    private final Vector     _globals;
    private final Object[][] _frames;

    // *******************************************************************

    private DynamicEnvironment(
        Vector     globals,
        Object[][] frames
    )
    {
        _globals = globals;
        _frames  = frames;
    }

    private static DynamicEnvironment create(
        DynamicEnvironment parent,
        int                size
    )
    {
        Object[][] oldFrames = parent._frames;
        int       newIndex  = oldFrames.length;
        Object[][] newFrames = new Object[newIndex + 1][];

        if (newIndex > 0)
        {
            System.arraycopy(
                oldFrames, 0,
                newFrames, 0,
                newIndex
            );
        }

        newFrames[newIndex] = new Object[size];

        return new DynamicEnvironment(
            parent._globals,
            newFrames
        );
    }

    private static DynamicEnvironment create(
        DynamicEnvironment parent,
        Arity              arity,
        int                frameSize,
        IList               values
    ) throws PairExpected, ListExpected
    {
        DynamicEnvironment result = create(
            parent,
            frameSize
        );

        Object[] frame = result._frames[result._frames.length - 1];
        IList     rest  = values;

        for (int i = 0; i < arity.getMin(); i++)
        {
            frame[i] = rest.getHead();
            rest     = rest.getTail();
        }

        if (arity.allowMore())
        {
            frame[arity.getMin()] = rest;
        }

        return result;
    }


    public static DynamicEnvironment create()
    {
        return new DynamicEnvironment(
            new Vector(),
            new Object[0][]
        );
    }

    public DynamicEnvironment createChild(int size)
    {
        return create(this, size);
    }

    public DynamicEnvironment createChild(
        Arity             arity,
        int               frameSize,
        IList              values
    ) throws ListExpected, PairExpected
    {
        return create(this, arity, frameSize, values);
    }


    // *** Envrionment access ************************************************

    // *** value access (runtime) ***

	public Object assign(Reference ref, Object value)
	{
		int level = ref.getLevel();
		int index = ref.getIndex();

		Object result = null;

		if (level > 0)
		{
			result = _frames[level - 1][index];
			_frames[level - 1][index] = value;
		}
		else
		{
			try
			{
				result = _globals.elementAt(index);
				_globals.setElementAt(value, index);
			}
			catch (ArrayIndexOutOfBoundsException e)
			{
				_globals.setSize(index + 1);
				_globals.setElementAt(value, index);
			}
		}

		return (result != null) ? result : value;
	}

    public Object lookupNoThrow(Reference ref)
    {
        try
        {
            int level = ref.getLevel();
            int index = ref.getIndex();
            
            if (level > 0)
            {
                return _frames[level - 1][index];
            }
            else
            {
                return _globals.elementAt(index);
            }
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            return null;
        }
    }

    public Object lookup(Reference ref)
        throws RuntimeError
    {
        Object result = lookupNoThrow(ref);

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
