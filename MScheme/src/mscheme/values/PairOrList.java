/* The implementation of Scheme's pairs.
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

import mscheme.exceptions.ImmutableException;
import mscheme.exceptions.ListExpected;
import mscheme.exceptions.PairExpected;
import mscheme.exceptions.SchemeException;


final class PairOrList
    extends Compound
    implements List, Pair, Outputable, Comparable
{
    public final static String id
        = "$Id$";


    private Object _first;
    private Object _second;

    private PairOrList(boolean isConst, Object first, Object second)
    {
        super(isConst);

        _first  = first;
        _second = second;
    }


    public static List prepend(Object head, List tail)
    {
        return new PairOrList(false, head, tail);
    }

    public static Pair create(Object first, Object second)
    {
        return new PairOrList(false, first, second);
    }

    public static Pair createConst(Object first, Object second)
    {
        return new PairOrList(
        	true,
			ValueTraits.getConst(first),
			ValueTraits.getConst(second));
    }


    // implementation of Value

    public void outputOn(Writer destination, boolean doDisplay)
        throws IOException
    {
        destination.write('(');

        Object  current = this;
        Object  delayed = this;
        boolean advance = false;
        boolean first   = true;

        while (current instanceof Pair)
        {
            if (!first)
            {
                destination.write(' ');
            }
            else
            {
                first = false;
            }

            Pair currentPair = (Pair)current;

			ValueTraits.output(destination, doDisplay, currentPair.getFirst());

            current = currentPair.getSecond();

            if (advance)
            {
                delayed = ((Pair)delayed).getSecond();
                
                if (delayed == current)
                {
                    destination.write(" . [ cyclic ])");
                    return;
                }
            }
            advance ^= true;
        }

        if (!(current instanceof Empty))
        {
            // 'this' is an improper list

            destination.write(" . ");

			ValueTraits.output(destination, doDisplay, current);
        }

        destination.write(')');
    }

    public boolean equal(Object other)
    {
        try
        {
            Pair otherPair = (Pair)other;

            return
				ValueTraits.equal(getFirst (), otherPair.getFirst ()) &&
				ValueTraits.equal(getSecond(), otherPair.getSecond());
        }
        catch (ClassCastException e)
        { }

        return false;
    }

    public Object getCompiled(StaticEnvironment compilationEnv)
        throws SchemeException
    {
        List list = toList();

        return
			ValueTraits.getSyntax(
				compilationEnv,
				list.getHead())
            .translate(
                compilationEnv,
                list.getTail()
            );
    }


    // implementation of Compound

    protected final Object getConstCopy()
    {
        return createConst(
            getFirst(),
            getSecond()
        );
    }


    // implementation of Pair

    public final boolean isPair()
    {
        return true;
    }

    public final Pair toPair()
    {
        return this;
    }

    public final Object getFirst()
    {
        return _first;
    }

    public final void setFirst(Object first)
        throws ImmutableException
    {
        modify();
        _first = first;
    }

    public final Object getSecond()
    {
        return _second;
    }

    public final void setSecond(Object second)
        throws ImmutableException
    {
        modify();
        _second = second;
    }


    // implementation of List

    public boolean isList()
    {
        try {
            Object hare = getSecond();

            if (hare instanceof Empty)
            {
                return true;
            }

            Object tortoise = hare;
            do {
                hare = ((Pair)hare).getSecond();
                if (hare instanceof Empty)
                {
                    return true;
                }

                hare = ((Pair)hare).getSecond();
                if (hare instanceof Empty)
                {
                    return true;
                }

                tortoise = ((Pair)tortoise).getSecond();
            } while (hare != tortoise);

            return false;
        }
        catch (ClassCastException e)
        {
            return false; // improper list
        }
    }

    public List toList()
        throws ListExpected
    {
        return isList() ? this : super.toList();
    }

    public boolean isEmpty()
    {
        return false;
    }

    public List getCopy()
    {
        try
        {
            PairOrList result  = new PairOrList(false, getHead(), null);
            PairOrList current = result;

            for (
                List tail = getTail();
                !tail.isEmpty();
                tail = tail.getTail()
            )
            {
                PairOrList next = new PairOrList(
                    false,
                    tail.getHead(),
                    null
                );
                current._second = next;
                current = next;
            }
            current._second = Empty.create();

            return result;
        }
        catch (PairExpected e)
        {
            throw new RuntimeException(
                "unexpected PairExpected"
            );
        }
    }

    public Object getHead()
    {
        return getFirst();
    }

    public List getTail()
    {
        return (List)getSecond();
    }

    public int getLength()
    {
        try
        {
            int result = 1;

            for (
                List tail = getTail();
                !tail.isEmpty();
                tail = tail.getTail()
            )
            {
                ++result;
            }

            return result;
        }
        catch (PairExpected e)
        {
            throw new RuntimeException(
                "unexpected PairExpected"
            );
        }
    }

    public final List getReversed()
    {
        try
        {
            List result = Empty.create();

            for (
                List rest = this;
                !rest.isEmpty();
                rest = rest.getTail()
            )
            {
                result = ListFactory.prepend(
                             rest.getHead(),
                             result
                         );
            }

            return result;
        }
        catch (PairExpected e)
        {
            throw new RuntimeException(
                      "unexpected PairExpected"
                  );
        }
    }

    public Object[] getCompiledArray(StaticEnvironment compilationEnv)
        throws SchemeException
    {
        return getCompiledArray(compilationEnv, 0);
    }

    public Object[] getCompiledArray(StaticEnvironment compilationEnv, int index)
        throws SchemeException
    {
        Object   compiledHead = ValueTraits.getCompiled(compilationEnv, getHead());
        Object[] result       = getTail().getCompiledArray(
                                    compilationEnv,
                                    index + 1);

       	result[index] = compiledHead;
        
        return result;
    }


    public Object[] getArray()
    {
    	return getArray(0);
    }

	public Object[] getArray(int index)
	{
		Object[] result = getTail().getArray(index + 1);

		result[index] = getHead();				 

		return result;
	}
}
