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
    extends junit.framework.TestCase {

  Object firstElement;

  Object secondElement;

  Object lastElement;

  IList emptyList;

  IList occupiedList;

  int occupiedListLength;

  public TestList(String name) {
    super(name);
  }

  protected void setUp() {
    emptyList = ListFactory.create();

    firstElement = "x";
    secondElement = "y";
    lastElement = secondElement;
    occupiedList = ListFactory.create(firstElement, secondElement);
    occupiedListLength = 2;
  }

  protected void tearDown() {
    firstElement = null;
    secondElement = null;
    emptyList = null;
    occupiedList = null;
  }

  public void testTestValues() {
    assertTrue(occupiedListLength >= 2);

    assertNotSame(firstElement, secondElement);

    assertNotSame(firstElement, lastElement);
  }

  public void testEmptyIsUnique()
      throws Exception {
    assertSame("empty isn't unique", emptyList, ListFactory.create());
  }

  public void testOccupiedList()
      throws Exception {
    assertNotSame("occupied list equals (==) empty list", occupiedList, emptyList);

    assertSame("toPair returned somethig wrong", ValueTraits.toConstPair(
        occupiedList).getFirst(), occupiedList.getHead());
  }

  public void testIsEmpty() {
    assertTrue(emptyList.isEmpty());

    assertFalse(occupiedList.isEmpty());
  }

  public void testGetLength()
      throws Exception {
    assertEquals(0, emptyList.getLength());

    assertEquals(occupiedList.getLength(), occupiedListLength);
  }

  public void testGetHead()
      throws Exception {
    try {
      emptyList.getHead();
      fail("PairExpected expected");
    } catch (PairExpected e) {
    }

    assertSame("getHead failed", occupiedList.getHead(), firstElement);
  }

  public void testGetTail()
      throws Exception {
    try {
      assertNotNull(emptyList.getTail());
      fail("PairExpected expected");
    } catch (PairExpected e) {
    }

    assertSame("getTail failed", occupiedList.getTail().getHead(), secondElement);
  }

  public void testGetReversed()
      throws Exception {
    assertSame("failed on emptyList", emptyList.getReversed(), emptyList);

    assertEquals("length mismatch ", occupiedList.getReversed().getLength(), occupiedListLength);

    assertSame("previous last isn't first now", occupiedList.getReversed()
        .getHead(), lastElement);
  }

  public void testGetCodeList() {
    // ...
  }
}