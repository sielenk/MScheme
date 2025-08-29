/* 
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

package mscheme.tests;

import java.io.StringReader;
import java.io.StringWriter;
import mscheme.environment.Environment;
import mscheme.exceptions.TypeError;
import mscheme.values.InputPort;
import mscheme.values.ListFactory;
import mscheme.values.OutputPort;
import mscheme.values.ScmNumber;
import mscheme.values.ScmString;
import mscheme.values.ScmVector;
import mscheme.values.ValueTraits;
import mscheme.values.functions.CallCCFunction;

public class TestValue
    extends junit.framework.TestCase {

  public TestValue(String name) {
    super(name);
  }

  protected void setUp()
      throws Exception {
  }

  protected void tearDown() {
  }


  private int countTypes(Object v) {
    int count = 0;

    if (ValueTraits.INSTANCE.isList(v)) {
      ++count;
    }

    if (ValueTraits.INSTANCE.isScmBoolean(v)) {
      ++count;
    }
    if (ValueTraits.INSTANCE.isPair(v)) {
      ++count;
    }
    if (ValueTraits.INSTANCE.isSymbol(v)) {
      ++count;
    }
    if (ValueTraits.INSTANCE.isScmNumber(v)) {
      ++count;
    }
    if (ValueTraits.INSTANCE.isScmChar(v)) {
      ++count;
    }
    if (ValueTraits.INSTANCE.isScmString(v)) {
      ++count;
    }
    if (ValueTraits.INSTANCE.isScmVector(v)) {
      ++count;
    }
    if (ValueTraits.INSTANCE.isPort(v)) {
      ++count;
    }
    if (ValueTraits.INSTANCE.isFunction(v)) {
      ++count;
    }

    return count;
  }

  private int countCasts(Object v) {
    int count = 0;

    try {
      ValueTraits.INSTANCE.toList(v);
      ++count;
    } catch (TypeError e) {
    }

    try {
      ValueTraits.INSTANCE.toConstPair(v);
      ++count;
    } catch (TypeError e) {
    }
    try {
      ValueTraits.INSTANCE.toSymbol(v);
      ++count;
    } catch (TypeError e) {
    }
    try {
      ValueTraits.INSTANCE.toScmNumber(v);
      ++count;
    } catch (TypeError e) {
    }
    try {
      ValueTraits.INSTANCE.toScmChar(v);
      ++count;
    } catch (TypeError e) {
    }
    try {
      ValueTraits.INSTANCE.toScmString(v);
      ++count;
    } catch (TypeError e) {
    }
    try {
      ValueTraits.INSTANCE.toScmVector(v);
      ++count;
    } catch (TypeError e) {
    }
    try {
      ValueTraits.INSTANCE.toInputPort(v);
      ++count;
    } catch (TypeError e) {
    }
    try {
      ValueTraits.INSTANCE.toOutputPort(v);
      ++count;
    } catch (TypeError e) {
    }

    try {
      ValueTraits.INSTANCE.toEnvironment(v);
      ++count;
    } catch (TypeError e) {
    }
    try {
      ValueTraits.INSTANCE.toStaticEnvironment(v);
      ++count;
    } catch (TypeError e) {
    }

    return count;
  }

  private void commonTests(Object v, int castCount) {
    assertTrue(ValueTraits.INSTANCE.isTrue(v));

    assertEquals(1, countTypes(v));
    assertEquals(countCasts(v), castCount);
  }

  private void commonTests(Object v) {
    commonTests(v, 1);
  }


  public void testFalse()
      throws Exception {
    final Object False = ValueTraits.FALSE;

    assertFalse(ValueTraits.INSTANCE.isTrue(False));

    assertEquals(1, countTypes(False));
    assertEquals(0, countCasts(False));

    assertTrue(ValueTraits.INSTANCE.isScmBoolean(False));
  }

  public void testTrue()
      throws Exception {
    final Object True = ValueTraits.TRUE;

    assertTrue(ValueTraits.INSTANCE.isTrue(True));

    assertEquals(1, countTypes(True));
    assertEquals(0, countCasts(True));

    assertTrue(ValueTraits.INSTANCE.isScmBoolean(True));
  }

  public void testEmpty()
      throws Exception {
    final Object empty = ListFactory.INSTANCE.create();

    assertTrue(ValueTraits.INSTANCE.isTrue(empty));

    assertEquals(1, countTypes(empty)); // List
    assertEquals(1, countCasts(empty));

    assertTrue(ValueTraits.INSTANCE.isEmpty(empty));

    assertTrue(ValueTraits.INSTANCE.isList(empty));
    assertSame(ValueTraits.INSTANCE.toList(empty), empty);
  }

  public void testPair()
      throws Exception {
    final Object pair = ListFactory.INSTANCE.createPair(
        ValueTraits.TRUE,
        ValueTraits.TRUE
    );

    commonTests(pair);
    assertTrue(ValueTraits.INSTANCE.isPair(pair));
    assertSame(ValueTraits.INSTANCE.toConstPair(pair), pair);
  }

  public void testList()
      throws Exception {
    final Object list = ListFactory.INSTANCE.create(
        ValueTraits.TRUE
    );

    assertTrue(ValueTraits.INSTANCE.isTrue(list));

    assertEquals(2, countTypes(list)); // List and Pair
    assertEquals(2, countCasts(list));

    assertTrue(ValueTraits.INSTANCE.isPair(list));
    assertSame(ValueTraits.INSTANCE.toConstPair(list), list);

    assertTrue(ValueTraits.INSTANCE.isList(list));
    assertSame(ValueTraits.INSTANCE.toList(list), list);
  }

  public void testSymbol()
      throws Exception {
    final Object symbol = "test";

    commonTests(symbol);
    assertTrue(ValueTraits.INSTANCE.isSymbol(symbol));
    assertSame(ValueTraits.INSTANCE.toSymbol(symbol), symbol);
  }

  public void testFunction()
      throws Exception {
    final Object function = CallCCFunction.INSTANCE;

    commonTests(function, 0);
    assertTrue(ValueTraits.INSTANCE.isFunction(function));
  }

  public void testNumber()
      throws Exception {
    final Object number = ScmNumber.Companion.create(49875);

    commonTests(number);
    assertTrue(ValueTraits.INSTANCE.isScmNumber(number));
    assertSame(ValueTraits.INSTANCE.toScmNumber(number), number);
  }

  public void testChar()
      throws Exception {
    final Object character = ValueTraits.INSTANCE.toScmChar('a');

    commonTests(character);
    assertTrue(ValueTraits.INSTANCE.isScmChar(character));
    assertSame(ValueTraits.INSTANCE.toScmChar(character), character);
  }

  public void testString()
      throws Exception {
    final Object string = ScmString.Companion.create("Hallo !");

    commonTests(string);
    assertTrue(ValueTraits.INSTANCE.isScmString(string));
    assertSame(ValueTraits.INSTANCE.toScmString(string), string);
  }

  public void testVector()
      throws Exception {
    final Object vector = ScmVector.Companion.create();

    commonTests(vector);
    assertTrue(ValueTraits.INSTANCE.isScmVector(vector));
    assertSame(ValueTraits.INSTANCE.toScmVector(vector), vector);
  }

  public void testOutputPort()
      throws Exception {
    final Object port = OutputPort.Companion.create(new StringWriter());

    commonTests(port);
    assertTrue(ValueTraits.INSTANCE.isPort(port));
    assertSame(ValueTraits.INSTANCE.toOutputPort(port), port);
  }

  public void testInputPort()
      throws Exception {
    final Object port = InputPort.Companion.create(new StringReader(""));

    commonTests(port);
    assertTrue(ValueTraits.INSTANCE.isPort(port));
    assertSame(ValueTraits.INSTANCE.toInputPort(port), port);
  }

  public void testEnvironment()
      throws Exception {
    final Object environment = Environment.Companion.getEmpty();

    assertTrue(ValueTraits.INSTANCE.isTrue(environment));

    assertEquals(0, countTypes(environment));
    assertEquals(1, countCasts(environment));

    assertSame(ValueTraits.INSTANCE.toEnvironment(environment), environment);
  }

  public void testStaticEnvironment()
      throws Exception {
    final Object environment = Environment.Companion.getEmpty().getStatic();

    assertTrue(ValueTraits.INSTANCE.isTrue(environment));

    assertEquals(0, countTypes(environment));
    assertEquals(1, countCasts(environment));

    assertSame(ValueTraits.INSTANCE.toStaticEnvironment(environment), environment);
  }


  private int eqHelper(Object fst, Object snd) {
    boolean eq = ValueTraits.INSTANCE.eq(fst, snd);
    boolean eqv = ValueTraits.INSTANCE.eqv(fst, snd);
    boolean equal = ValueTraits.INSTANCE.equal(fst, snd);

    // reflexivity
    assertTrue(ValueTraits.INSTANCE.eq(fst, fst));
    assertTrue(ValueTraits.INSTANCE.eq(snd, snd));
    assertTrue(ValueTraits.INSTANCE.eqv(fst, fst));
    assertTrue(ValueTraits.INSTANCE.eqv(snd, snd));
    assertTrue(ValueTraits.INSTANCE.equal(fst, fst));
    assertTrue(ValueTraits.INSTANCE.equal(snd, snd));

    // symmetry
    assertEquals(eq, ValueTraits.INSTANCE.eq(snd, fst));
    assertEquals(eqv, ValueTraits.INSTANCE.eqv(snd, fst));
    assertEquals(equal, ValueTraits.INSTANCE.equal(snd, fst));

    assertTrue(!eq | eqv); // aka. eq  -> eqv
    assertTrue(!eqv | equal); // aka. eqv -> equal

    if (eq) {
      return 3;
    } else if (eqv) {
      return 2;
    } else if (equal) {
      return 1;
    } else {
      return 0;
    }
  }

  public void testEq()
      throws Exception {
    Object u = "u";
    Object v = "v";

    // eq equivalent values

    assertEquals(3, eqHelper(v, v));

    assertEquals(3, eqHelper(
        ValueTraits.TRUE,
        ValueTraits.TRUE
    ));

    assertEquals(3, eqHelper(
        ValueTraits.FALSE,
        ValueTraits.FALSE
    ));

    assertEquals(3, eqHelper(
        "a",
        "a"
    ));

    assertEquals(3, eqHelper(
        ListFactory.INSTANCE.create(),
        ListFactory.INSTANCE.create()
    ));

    // eqv equivalent values

    assertTrue(
        eqHelper(
            ScmNumber.Companion.create(7123645),
            ScmNumber.Companion.create(7123645)
        ) >= 2
    );

    assertTrue(
        eqHelper(
            ValueTraits.INSTANCE.toScmChar('u'),
            ValueTraits.INSTANCE.toScmChar('u')
        ) >= 2
    );

    // equal equivalent but eqv unspec. values

    assertTrue(
        eqHelper(
            ScmVector.Companion.create(),
            ScmVector.Companion.create()
        ) >= 1
    );

    assertTrue(
        eqHelper(
            ScmVector.Companion.create(5, v),
            ScmVector.Companion.create(5, v)
        ) >= 1
    );

    assertTrue(
        eqHelper(
            ScmString.Companion.create(""),
            ScmString.Companion.create("")
        ) >= 1
    );

    assertTrue(
        eqHelper(
            ScmString.Companion.create("Hallo"),
            ScmString.Companion.create("Hallo")
        ) >= 1
    );

    // equal equivalent but eqv different values

    assertEquals(1, eqHelper(
        ListFactory.INSTANCE.createPair(v, v),
        ListFactory.INSTANCE.createPair(v, v)
    ));

    // different values

    assertEquals(0, eqHelper(
        ValueTraits.TRUE,
        ValueTraits.FALSE
    ));

    assertEquals(0, eqHelper(
        "u",
        "v"
    ));

    assertEquals(0, eqHelper(
        ScmVector.Companion.create(5, u),
        ScmVector.Companion.create(5, v)
    ));

    assertEquals(0, eqHelper(
        ScmVector.Companion.create(7, v),
        ScmVector.Companion.create(5, v)
    ));

    assertEquals(0, eqHelper(
        ScmString.Companion.create("Hallo 1"),
        ScmString.Companion.create("Hallo 2")
    ));
  }
}
