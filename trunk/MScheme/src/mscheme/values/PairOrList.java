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
import java.io.StringWriter;
import java.io.Writer;

import mscheme.environment.StaticEnvironment;
import mscheme.exceptions.ImmutableException;
import mscheme.exceptions.ListExpected;
import mscheme.exceptions.PairExpected;
import mscheme.exceptions.SchemeException;
import mscheme.syntax.ProcedureCall;
import mscheme.syntax.ITranslator;


class ConstPairOrList
	extends PairOrList
{
    public final static String CVS_ID
    	= "$Id$";


    private final Object _first;
    private final Object _second;

	ConstPairOrList(Object first, Object second)
	{
        _first  = ValueTraits.getConst(first);
        _second = ValueTraits.getConst(second);
	}

	public void setFirst(Object first)
		throws ImmutableException
	{
		throw new ImmutableException(this);
	}

    public Object getFirst()
    {
        return _first;
    }
   

    public void setSecond(Object second)
        throws ImmutableException
    {
		throw new ImmutableException(this);
    }
    
    public Object getSecond()
    {
        return _second;
    }
}


class MutablePairOrList
	extends PairOrList
	implements IMutable
{
    public final static String CVS_ID
    	= "$Id$";


    private Object _first;
    private Object _second;

	MutablePairOrList(Object first, Object second)
	{
        _first  = first;
        _second = second;
	}


    public void setFirst(Object first)
    {
        _first = first;
    }

    public Object getFirst()
    {
        return _first;
    }
   

    public void setSecond(Object second)
    {
        _second = second;
    }

    public Object getSecond()
    {
        return _second;
    }

    
    public Object getConst()
    {
        return new ConstPairOrList(_first, _second);
    }
}


public abstract class PairOrList
    implements IList, IPair, ICompileable, IComparable, IOutputable
{
    public final static String CVS_ID
        = "$Id$";


    protected PairOrList()
    { }


    public static IList prepend(Object head, IList tail)
    {
        return new MutablePairOrList(head, tail);
    }

    public static IList prependConst(Object head, IList tail)
    {
        return new ConstPairOrList(head, tail);
    }

    public static IPair create(Object first, Object second)
    {
        return new MutablePairOrList(first, second);
    }

    public static IPair createConst(Object first, Object second)
    {
        return new ConstPairOrList(first, second);
    }


    // implementation of Outputable

    public void outputOn(Writer destination, boolean doDisplay)
        throws IOException
    {
        destination.write('(');

        Object  current = this;
        Object  delayed = this;
        boolean advance = false;
        boolean first   = true;

        while (current instanceof IPair)
        {
            if (!first)
            {
                destination.write(' ');
            }
            else
            {
                first = false;
            }

            IPair currentPair = (IPair)current;

			ValueTraits.output(destination, doDisplay, currentPair.getFirst());

            current = currentPair.getSecond();

            if (advance)
            {
                delayed = ((IPair)delayed).getSecond();
                
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
        if (!(other instanceof IPair))
        {
            return false;
        }
            
        IPair otherPair = (IPair)other;

        return
			ValueTraits.equal(getFirst (), otherPair.getFirst ()) &&
			ValueTraits.equal(getSecond(), otherPair.getSecond());
    }

    public ITranslator getTranslator(StaticEnvironment compilationEnv)
    	throws SchemeException
	{
	    return ProcedureCall.create(this);
	}

    public Object getCompiled(StaticEnvironment compilationEnv)
        throws SchemeException
    {
        IList list = validate();

        return
			ValueTraits.getTranslator(
				compilationEnv,
				list.getHead())
            .translate(
                compilationEnv,
                list.getTail()
            );
    }


    // implementation of Pair

    abstract public Object getFirst();

    abstract public void setFirst(Object first)
        throws ImmutableException;

    abstract public Object getSecond();

    abstract public void setSecond(Object second)
        throws ImmutableException;


    // implementation of List

    public boolean isValid()
    {
        Object hare = getSecond();

        if (hare instanceof Empty)
        {
            return true;
        }

        Object tortoise = hare;
        do {
            if (hare instanceof IPair)
            {
                hare = ((IPair)hare).getSecond();
            }
            else
            {
            	return (hare instanceof Empty);
            }

			if (hare instanceof IPair)
			{
            	hare = ((IPair)hare).getSecond();
           	}
           	else
           	{
           		return (hare instanceof Empty);
            }

            tortoise = ((IPair)tortoise).getSecond();
        } while (hare != tortoise);

        return false;
    }

    public IList validate()
        throws ListExpected
    {
        if (!isValid())
        {
            throw new ListExpected(this);
        }
        
        return this;
    }

    public boolean isEmpty()
    {
        return false;
    }

    public IList getCopy()
    {
        try
        {
            MutablePairOrList result  = new MutablePairOrList(getHead(), null);
            MutablePairOrList current = result;

            for (
                IList tail = getTail();
                !tail.isEmpty();
                tail = tail.getTail()
            )
            {
                MutablePairOrList next = new MutablePairOrList(
                    tail.getHead(),
                    null
                );
                current.setSecond(next);
                current = next;
            }
            current.setSecond(ListFactory.create());

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

    public IList getTail()
    {
        return (IList)getSecond();
    }

    public int getLength()
    {
        try
        {
            int result = 1;

            for (
                IList tail = getTail();
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

    public IList getReversed()
    {
        try
        {
            IList result = ListFactory.create();

            for (
                IList rest = this;
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


	public String toString()
	{
		StringWriter out = new StringWriter();
		try
        {
            outputOn(out, true);
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
		return out.toString();
	}
}
