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

package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.Code;

import MScheme.environment.StaticEnvironment;
import MScheme.code.CodeList;

import MScheme.exceptions.*;


final class PairOrList
    extends Compound
    implements ExtendedList, Pair
{
    public final static String id
        = "$Id$";


    private Value _first;
    private Value _second;

    private PairOrList(boolean isConst, Value first, Value second)
    {
        super(isConst);

        _first  = first;
        _second = second;
    }


    public static List prepend(Value head, List tail)
    {
        return new PairOrList(false, head, tail);
    }

    public static Pair create(Value first, Value second)
    {
        return new PairOrList(false, first, second);
    }

    public static Pair createConst(Value first, Value second)
    {
        return new PairOrList(true, first.getConst(), second.getConst());
    }


    // implementation of Value

    private void put(Writer destination, boolean doDisplay)
        throws IOException
    {
        destination.write('(');

        Value current = this;
        while (current instanceof Pair)
        {
            // 'this' is the first element of the list
            // and needs no leading space
            // (the opening parenthesis is a delimiter)
            if (current != this)
            {
                destination.write(' ');
            }

            Pair currentPair = (Pair)current;

            if (doDisplay)
            {
                currentPair.getFirst().display(destination);
            }
            else
            {
                currentPair.getFirst().write(destination);
            }

            current = currentPair.getSecond();
        }

        if (!current.isEmpty())
        {
            // 'this' is an improper list

            destination.write(" . ");

            if (doDisplay)
            {
                current.display(destination);
            }
            else
            {
                current.write(destination);
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

    public boolean equal(Value other)
    {
        try
        {
            Pair otherPair = (Pair)other;

            return
                (getFirst ().equal(otherPair.getFirst ())) &&
                (getSecond().equal(otherPair.getSecond()));
        }
        catch (ClassCastException e)
        { }

        return false;
    }

    public Code getCode(StaticEnvironment compilationEnv)
        throws SchemeException
    {
        return
            getHead()
            .getSyntax(compilationEnv)
            .translate(
                compilationEnv,
                getTail()
            );
    }


    // implementation of Compound

    public Value getCopy()
    {
        Value second = getSecond();

        if (second.isPair())
        {
            second = second.getCopy();
        }

        return create(
            getFirst(),
            second
        );
    }

    protected final Value getConstCopy()
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

    public final Value getFirst()
    {
        return _first;
    }

    public final void setFirst(Value first)
        throws ImmutableException
    {
        modify();
        _first = first;
    }

    public final Value getSecond()
    {
        return _second;
    }

    public final void setSecond(Value second)
        throws ImmutableException
    {
        modify();
        _second = second;
    }


    // implementation of List

    public boolean isList()
    {
        return getSecond().isList();
    }

    public List toList()
        throws ListExpected
    {
        return isList() ? this : super.toList();
    }

    public Value getHead()
    {
        return getFirst();
    }

    public List getTail()
        throws ListExpected
    {
        return getSecond().toList();
    }

    public int getLength()
        throws ListExpected
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
        throws ListExpected
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

    public Code[] getCodeArray(StaticEnvironment compilationEnv)
        throws SchemeException
    {
        return getCodeArray(compilationEnv, 0);
    }

    public Code[] getCodeArray(StaticEnvironment compilationEnv, int index)
        throws SchemeException
    {
        Code         compiledHead = getHead().getCode(compilationEnv);
        ExtendedList tail         = (ExtendedList)getTail();
        Code[]       result       = tail.getCodeArray(
                                        compilationEnv,
                                        index + 1
                                    );

        result[index] = compiledHead;
        
        return result;
    }
}
