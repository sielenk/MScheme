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

package mscheme.values.functions;

import mscheme.exceptions.RuntimeError;
import mscheme.exceptions.TypeError;

import mscheme.values.List;

abstract class Reducer
{
    public final static String id
        = "$Id$";


    private final Object _initial;

    protected Reducer(Object initial)
    {
        _initial = initial;
    }

    protected abstract Object combine(Object fst, Object snd)
        throws RuntimeError, TypeError;


    public final Object reduceLeft(List list)
        throws RuntimeError, TypeError
    {
        if (list.isEmpty())
        {
            return _initial;
        }
        else
        {
            Object result = list.getHead();
        
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

    public final Object foldLeft(List list)
        throws RuntimeError, TypeError
    {
        Object result = _initial;
    
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

    private Object reduceRightHelper(List list)
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

    public final Object reduceRight(List list)
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

    public Object foldRight(List list)
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
