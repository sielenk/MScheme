/* A helper class for sequences and applications.
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

package MScheme.code;

import MScheme.Code;

import MScheme.exceptions.*;


public abstract class CodeList
{
    public final static String id
    = "$Id$";


    protected CodeList()
    { }


    // static creation functions

    public static CodeList prepend(
        Code     head,
        CodeList tail
    )
    {
        return new CodeListPair(head, tail);
    }

    public static CodeList create()
    {
        return CodeListEmpty.getInstance();
    }

    public static CodeList create(
        Code first
    )
    {
        return prepend(first, create());
    }

    public static CodeList create(
        Code first,
        Code second
    )
    {
        return prepend(first, create(second));
    }

    public static CodeList create(
        Code first,
        Code second,
        Code third
    )
    {
        return prepend(first, create(second, third));
    }

    public static CodeList create(
        Code first,
        Code second,
        Code third,
        Code fourth
    )
    {
        return prepend(first, create(second, third, fourth));
    }


    // abstract interface

    public abstract boolean  isEmpty();
    public abstract Code     getHead();
    public abstract CodeList getTail();
    public abstract CodeList getReversed();

    public abstract String toString();
}


final class CodeListPair
            extends CodeList
{
    public final static String id
    = "$Id$";


    private final Code     _head;
    private final CodeList _tail;

    CodeListPair(
        Code     head,
        CodeList tail
    )
    {
        _head = head;
        _tail = tail;
    }


    // implementation of CodeList

    public boolean isEmpty()
    {
        return false;
    }

    public Code getHead()
    {
        return _head;
    }

    public CodeList getTail()
    {
        return _tail;
    }

    public CodeList getReversed()
    {
        CodeList currentTail = this;
        CodeList result      = CodeList.create();

        while (!currentTail.isEmpty())
        {
            result = CodeList.prepend(
                         currentTail.getHead(),
                         result
                     );
            currentTail = currentTail.getTail();
        }

        return result;
    }

    public String toString()
    {
        StringBuffer buffer = new StringBuffer();

        for (CodeList current = this;;)
        {
            buffer.append(current.getHead().toString());
            current = current.getTail();
            if (current.isEmpty())
            {
                break;
            }
            buffer.append(' ');
        }

        return buffer.toString();
    }
}


final class CodeListEmpty
            extends CodeList
{
    public final static String id
    = "$Id$";


    // construction

    private CodeListEmpty()
    { }

    private final static CodeListEmpty
    _instance = new CodeListEmpty();

    static CodeListEmpty getInstance()
    {
        return _instance;
    }


    // implementation of CodeList

    public boolean isEmpty()
    {
        return true;
    }

    public Code getHead()
    {
        throw new RuntimeException("called getHead on empty CodeList!");
    }

    public CodeList getTail()
    {
        throw new RuntimeException("called getTail on empty CodeList!");
    }

    public CodeList getReversed()
    {
        return this;
    }

    public String toString()
    {
        return "()";
    }
}
