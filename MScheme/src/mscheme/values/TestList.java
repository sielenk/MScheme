/*
 * Some juint tests for lists. Copyright (C) 2001 Marvin H. Sielenkemper
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

import mscheme.exceptions.PairExpected;

public class TestList
        extends junit.framework.TestCase
{
    public final static String CVS_ID = "$Id$";

    Object firstElement;

    Object secondElement;

    Object lastElement;

    IList emptyList;

    IList occupiedList;

    int occupiedListLength;

    public TestList(String name)
    {
        super(name);
    }

    protected void setUp()
    {
        emptyList = ListFactory.create();

        firstElement = "x";
        secondElement = "y";
        lastElement = secondElement;
        occupiedList = ListFactory.create(firstElement, secondElement);
        occupiedListLength = 2;
    }

    protected void tearDown()
    {
        firstElement = null;
        secondElement = null;
        emptyList = null;
        occupiedList = null;
    }

    public void testTestValues()
    {
        assertTrue(occupiedListLength >= 2);

        assertTrue(firstElement != secondElement);

        assertTrue(firstElement != lastElement);
    }

    public void testEmptyIsUnique()
            throws Exception
    {
        assertTrue("empty isn't unique", emptyList == ListFactory.create());
    }

    public void testOccupiedList()
            throws Exception
    {
        assertTrue("occupied list equals (==) empty list",
                occupiedList != emptyList);

        assertTrue("toPair returned somethig wrong", ValueTraits.toConstPair(
                occupiedList).getFirst() == occupiedList.getHead());
    }

    public void testIsEmpty()
    {
        assertTrue(emptyList.isEmpty());

        assertTrue(!occupiedList.isEmpty());
    }

    public void testGetLength()
            throws Exception
    {
        assertTrue(emptyList.getLength() == 0);

        assertTrue(occupiedList.getLength() == occupiedListLength);
    }

    public void testGetHead()
            throws Exception
    {
        try
        {
            emptyList.getHead();
            fail("PairExpected expected");
        }
        catch (PairExpected e)
        {}

        assertTrue("getHead failed", occupiedList.getHead() == firstElement);
    }

    public void testGetTail()
            throws Exception
    {
        try
        {
            assertTrue(emptyList.getTail() != null);
            fail("PairExpected expected");
        }
        catch (PairExpected e)
        {}

        assertTrue("getTail failed",
                occupiedList.getTail().getHead() == secondElement);
    }

    public void testGetReversed()
            throws Exception
    {
        assertTrue("failed on emptyList", emptyList.getReversed() == emptyList);

        assertTrue("length mismatch ",
                occupiedList.getReversed().getLength() == occupiedListLength);

        assertTrue("previous last isn't first now", occupiedList.getReversed()
                .getHead() == lastElement);
    }

    public void testGetCodeList()
    {
    // ...
    }
}