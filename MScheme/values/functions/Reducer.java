/* A helper class for creating flexible ops from binary ones.
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

package MScheme.values.functions;

import MScheme.Value;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;

import MScheme.values.List;

abstract class Reducer
{
    public final static String id
        = "$Id$";


    private final Value _initial;

    protected Reducer(Value initial)
    {
        _initial = initial;
    }

    protected abstract Value combine(Value fst, Value snd)
        throws RuntimeError, TypeError;


    public final Value reduceLeft(List list)
        throws RuntimeError, TypeError
    {
        if (list.isEmpty())
        {
            return _initial;
        }
        else
        {
            Value result = list.getHead();
        
            for (
                List tail = list.getTail();
                !tail.isEmpty();
                tail   = tail.getTail()
            )
            {
                result = combine(result, tail.getHead());
            }

            return result;
        }
    }

    public final Value foldLeft(List list)
        throws RuntimeError, TypeError
    {
        Value result = _initial;
    
        for (
            List tail = list;
            !tail.isEmpty();
            tail   = tail.getTail()
        )
        {
            result = combine(result, tail.getHead());
        }

        return result;
    }

    private Value reduceRightHelper(List list)
        throws RuntimeError, TypeError
    {
        List tail = list.getTail();

        if (tail.isEmpty())
        {
            return list.getHead();
        }
        else
        {
            return combine(
                       list.getHead(),
                       reduceRightHelper(tail)
                   );
        }
    }

    public final Value reduceRight(List list)
        throws RuntimeError, TypeError
    {
        if (list.isEmpty())
        {
            return _initial;
        }
        else
        {
            return reduceRightHelper(list);
        }
    }

    public Value foldRight(List list)
        throws RuntimeError, TypeError
    {
        if (list.isEmpty())
        {
            return _initial;
        }
        else
        {
            return combine(
                       list.getHead(),
                       foldRight(list.getTail())
                   );
        }
    }
}
