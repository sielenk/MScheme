/*
 * The implementation of Scheme's pairs. Copyright (C) 2001 Marvin H.
 * Sielenkemper
 * 
 * This file is part of MScheme.
 * 
 * MScheme is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 * 
 * MScheme is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * MScheme; see the file COPYING. If not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

package mscheme.values;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Enumeration;

import mscheme.compiler.Compiler;
import mscheme.environment.StaticEnvironment;
import mscheme.exceptions.ListExpected;
import mscheme.exceptions.PairExpected;
import mscheme.exceptions.SchemeException;
import mscheme.syntax.ProcedureCall;
import mscheme.syntax.ITranslator;

class ConstPairOrList
        extends PairOrList
{
    public final static String CVS_ID = "$Id$";

    private final Object _first;

    private final Object _second;

    ConstPairOrList(Object first, Object second)
    {
        _first = ValueTraits.getConst(first);
        _second = ValueTraits.getConst(second);
    }

    public Object getFirst()
    {
        return _first;
    }

    public Object getSecond()
    {
        return _second;
    }
}

class MutablePairOrList
        extends PairOrList
        implements IMutablePair
{
    public final static String CVS_ID = "$Id$";

    private Object _first;

    private Object _second;

    MutablePairOrList(Object first, Object second)
    {
        _first = first;
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

class ListEnumerator
        implements Enumeration
{
    private Object _tortoise;

    private Object _hare;

    ListEnumerator(Object o)
    {
        _tortoise = o;
        _hare = (o instanceof IConstPair) ? ((IConstPair) o).getSecond() : o;
    }

    public boolean hasMoreElements()
    {
        return _hare != _tortoise;
    }

    public boolean isCyclic()
    {
        return _tortoise instanceof IConstPair;
    }

    public boolean isValid()
    {
        return _tortoise instanceof Empty;
    }

    public Object nextElement()
    {
        if (_tortoise instanceof IConstPair)
        {
            if (_hare instanceof IConstPair)
                _hare = ((IConstPair) _hare).getSecond();
            if (_hare instanceof IConstPair)
                _hare = ((IConstPair) _hare).getSecond();

            IConstPair pair = (IConstPair) _tortoise;
            _tortoise = pair.getSecond();
            return pair.getFirst();
        }
        else
            return _tortoise;
    }
}

public abstract class PairOrList
        implements IConstPair, IList, ICompileable, IComparable, IOutputable
{
    public final static String CVS_ID = "$Id$";

    protected PairOrList()
    {}

    public static IList prepend(Object head, IList tail)
    {
        return new MutablePairOrList(head, tail);
    }

    public static IList prependConst(Object head, IList tail)
    {
        return new ConstPairOrList(head, tail);
    }

    public static IMutablePair create(Object first, Object second)
    {
        return new MutablePairOrList(first, second);
    }

    public static IConstPair createConst(Object first, Object second)
    {
        return new ConstPairOrList(first, second);
    }

    // implementation of Outputable

    public void outputOn(Writer destination, boolean doDisplay)
            throws IOException
    {
        destination.write('(');

        ListEnumerator enumerator = new ListEnumerator(this);
        for (boolean first = true; enumerator.hasMoreElements(); first = false)
        {
            if (!first)
                destination.write(' ');

            ValueTraits
                    .output(destination, doDisplay, enumerator.nextElement());
        }

        if (!enumerator.isValid())
        {
            if (enumerator.isCyclic())
                destination.write(" . [ cyclic ]");
            else
            {
                destination.write(" . ");
                ValueTraits.output(destination, doDisplay, enumerator
                        .nextElement());
            }
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
        if (!(other instanceof PairOrList))
        {
            return false;
        }

        PairOrList otherPair = (PairOrList) other;

        return ValueTraits.equal(getFirst(), otherPair.getFirst())
                && ValueTraits.equal(getSecond(), otherPair.getSecond());
    }

    public ITranslator getTranslator(StaticEnvironment compilationEnv)
            throws SchemeException
    {
        return ProcedureCall.create(this);
    }

    public Object getForceable(StaticEnvironment compilationEnv)
            throws SchemeException
    {
        IList list = validate();

        return new Compiler(compilationEnv).getTranslator(list.getHead())
                .translate(compilationEnv, list.getTail());
    }

    // implementation of List

    public boolean isValid()
    {
        Object hare = getSecond();

        if (hare instanceof Empty)
            return true;

        Object tortoise = hare;
        do
        {
            if (hare instanceof IConstPair)
                hare = ((IConstPair) hare).getSecond();
            else
                return (hare instanceof Empty);

            if (hare instanceof IConstPair)
                hare = ((IConstPair) hare).getSecond();
            else
                return (hare instanceof Empty);

            tortoise = ((IConstPair) tortoise).getSecond();
        }
        while (hare != tortoise);

        return false;
    }

    public IList validate()
            throws ListExpected
    {
        if (!isValid())
            throw new ListExpected(this);

        return this;
    }

    public boolean isEmpty()
    {
        return false;
    }

    public IList getCopy()
    {
        final Object empty = ListFactory.create();

        ListEnumerator enumerator = new ListEnumerator(this);
        final MutablePairOrList result = new MutablePairOrList(enumerator.nextElement(), empty);
        MutablePairOrList current = result;
        while (enumerator.hasMoreElements())
        {
            MutablePairOrList next = new MutablePairOrList(enumerator.nextElement(), empty);
            current.setSecond(next);
            current = next;                    
        }

        return result;                
    }

    public Object getHead()
    {
        return getFirst();
    }

    public IList getTail()
    {
        return (IList) getSecond();
    }

    public int getLength()
    {
        try
        {
            int result = 1;

            for (IList tail = getTail(); !tail.isEmpty(); tail = tail.getTail())
            {
                ++result;
            }

            return result;
        }
        catch (PairExpected e)
        {
            throw new RuntimeException("unexpected PairExpected");
        }
    }

    public IList getReversed()
    {
        try
        {
            IList result = ListFactory.create();

            for (IList rest = this; !rest.isEmpty(); rest = rest.getTail())
            {
                result = ListFactory.prepend(rest.getHead(), result);
            }

            return result;
        }
        catch (PairExpected e)
        {
            throw new RuntimeException("unexpected PairExpected");
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
        Object compiledHead = new Compiler(compilationEnv)
                .getForceable(getHead());
        Object[] result = getTail().getCompiledArray(compilationEnv, index + 1);

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