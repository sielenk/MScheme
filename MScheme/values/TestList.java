/* Some juint tests for lists.
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

import MScheme.exceptions.*;

import MScheme.Value;


public class TestList
            extends junit.framework.TestCase
{
    public final static String id
    = "$Id$";


    Value firstElement;
    Value secondElement;
    Value lastElement;
    List  emptyList;
    List  occupiedList;
    int   occupiedListLength;

    public TestList(String name)
    {
        super(name);
    }

    protected void setUp()
    {
        emptyList     = Empty.create();

        firstElement  = Symbol.create("x");
        secondElement = Symbol.create("y");
        lastElement   = secondElement;
        occupiedList  = ListFactory.create(
                            firstElement,
                            secondElement
                        );
        occupiedListLength = 2;
    }

    protected void tearDown()
    {
        firstElement  = null;
        secondElement = null;
        emptyList     = null;
        occupiedList  = null;
    }


    public void testTestValues()
    {
        assert(
            occupiedListLength >= 2
        );

        assert(
            firstElement != secondElement
        );

        assert(
            firstElement != lastElement
        );
    }

    public void testEmptyIsUnique()
    throws Exception
    {
        assert(
            "empty isn't unique",
            emptyList == Empty.create()
        );
    }

    public void testOccupiedList()
    throws Exception
    {
        assert(
            "occupied list equals (==) empty list",
            occupiedList != emptyList
        );

        assert(
            "toPair returned somethig wrong",
            occupiedList.toPair().getFirst() == occupiedList.getHead()
        );
    }

    public void testIsEmpty()
    {
        assert(
            emptyList.isEmpty()
        );

        assert(
            !occupiedList.isEmpty()
        );
    }

    public void testGetLength()
    throws Exception
    {
        assert(
            emptyList.getLength() == 0
        );

        assert(
            occupiedList.getLength() == occupiedListLength
        );
    }

    public void testGetHead()
    throws Exception
    {
        try
        {
            Value dummy = emptyList.getHead();
            fail("PairExpected expected");
        }
        catch (PairExpected e)
        { }

        assert(
            "getHead failed",
            occupiedList.getHead() == firstElement
        );
    }

    public void testGetTail()
    throws Exception
    {
        try
        {
            assert(emptyList.getTail() != null);
            fail("PairExpected expected");
        }
        catch (PairExpected e)
        { }

        assert(
            "getTail failed",
            occupiedList.getTail().getHead() == secondElement
        );
    }

    public void testGetReversed()
    throws Exception
    {
        assert(
            "failed on emptyList",
            emptyList.getReversed() == emptyList
        );

        assert(
            "length mismatch ",
            occupiedList.getReversed().getLength() == occupiedListLength
        );

        assert(
            "previous last isn't first now",
            occupiedList.getReversed().getHead() == lastElement
        );
    }

    public void testGetCodeList()
    {
        // ...
    }
}
