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

import mscheme.values.Empty;
import mscheme.values.InputPort;
import mscheme.values.ListFactory;
import mscheme.values.OutputPort;
import mscheme.values.ScmNumber;
import mscheme.values.ScmString;
import mscheme.values.ScmVector;
import mscheme.values.Symbol;
import mscheme.values.ValueTraits;

import mscheme.values.functions.CallCCFunction;

public class TestValue
            extends junit.framework.TestCase
{
    public final static String id
        = "$Id$";


    public TestValue(String name)
    {
        super(name);
    }

    protected void setUp()
        throws Exception
    { }

    protected void tearDown()
    { }


    private int countTypes(Object v)
    {
        int count = 0;

        if (ValueTraits.isList      (v)) ++count;

        if (ValueTraits.isScmBoolean(v)) ++count;
        if (ValueTraits.isPair      (v)) ++count;
        if (ValueTraits.isSymbol    (v)) ++count;
        if (ValueTraits.isScmNumber (v)) ++count;
        if (ValueTraits.isScmChar   (v)) ++count;
        if (ValueTraits.isScmString (v)) ++count;
        if (ValueTraits.isScmVector (v)) ++count;
        if (ValueTraits.isPort      (v)) ++count;
        if (ValueTraits.isFunction  (v)) ++count;

        return count;
    }

    private int countCasts(Object v)
    {
        int count = 0;

        try { ValueTraits.toList             (v); ++count; } catch (TypeError e) { }

        try { ValueTraits.toPair             (v); ++count; } catch (TypeError e) { }
        try { ValueTraits.toSymbol           (v); ++count; } catch (TypeError e) { }
        try { ValueTraits.toScmNumber        (v); ++count; } catch (TypeError e) { }
        try { ValueTraits.toScmChar          (v); ++count; } catch (TypeError e) { }
        try { ValueTraits.toScmString        (v); ++count; } catch (TypeError e) { }
        try { ValueTraits.toScmVector        (v); ++count; } catch (TypeError e) { }
        try { ValueTraits.toInputPort        (v); ++count; } catch (TypeError e) { }
        try { ValueTraits.toOutputPort       (v); ++count; } catch (TypeError e) { }

        try { ValueTraits.toEnvironment      (v); ++count; } catch (TypeError e) { }
        try { ValueTraits.toStaticEnvironment(v); ++count; } catch (TypeError e) { }

        return count;
    }

    private void commonTests(Object v, int castCount)
    {
        assertTrue(ValueTraits.isTrue(v));

        assertTrue(countTypes(v) == 1);
        assertTrue(countCasts(v) == castCount);
    }

	private void commonTests(Object v)
	{
		commonTests(v, 1);
	}


    public void testFalse()
        throws Exception
    {
        final Object False = ValueTraits.FALSE;

        assertFalse(ValueTraits.isTrue(False));

        assertTrue(countTypes(False) == 1);
        assertTrue(countCasts(False) == 0);

        assertTrue(ValueTraits.isScmBoolean(False));
    }

    public void testTrue()
        throws Exception
    {
        final Object True = ValueTraits.TRUE;

        assertTrue(ValueTraits.isTrue(True));

        assertTrue(countTypes(True) == 1);
        assertTrue(countCasts(True) == 0);

        assertTrue(ValueTraits.isScmBoolean(True));
    }

    public void testEmpty()
        throws Exception
    {
        final Object empty = Empty.create();

        assertTrue(ValueTraits.isTrue(empty));

        assertTrue(countTypes(empty) == 1); // List
        assertTrue(countCasts(empty) == 1);

        assertTrue(empty instanceof Empty);

        assertTrue(ValueTraits.isList(empty));
        assertTrue(ValueTraits.toList(empty) == empty);
    }

    public void testPair()
        throws Exception
    {
        final Object pair = ListFactory.createPair(
            ValueTraits.TRUE,
			ValueTraits.TRUE
        );

        commonTests(pair);
        assertTrue(ValueTraits.isPair(pair));
        assertTrue(ValueTraits.toPair(pair) == pair);
    }

    public void testList()
    throws Exception
    {
        final Object list = ListFactory.create(
            ValueTraits.TRUE
        );

        assertTrue(ValueTraits.isTrue(list));

        assertTrue(countTypes(list) == 2); // List and Pair
        assertTrue(countCasts(list) == 2);

        assertTrue(ValueTraits.isPair(list));
        assertTrue(ValueTraits.toPair(list) == list);

        assertTrue(ValueTraits.isList(list));
        assertTrue(ValueTraits.toList(list) == list);
    }

    public void testSymbol()
    throws Exception
    {
        final Object symbol = Symbol.create("test");

        commonTests(symbol);
        assertTrue(ValueTraits.isSymbol(symbol));
        assertTrue(ValueTraits.toSymbol(symbol) == symbol);
    }

    public void testFunction()
    throws Exception
    {
        final Object function = CallCCFunction.INSTANCE;

        commonTests(function, 0);
        assertTrue(ValueTraits.isFunction(function));
    }

    public void testNumber()
        throws Exception
    {
        final Object number = ScmNumber.create(49875);

        commonTests(number);
        assertTrue(ValueTraits.isScmNumber(number));
        assertTrue(ValueTraits.toScmNumber(number) == number);
    }

    public void testChar()
        throws Exception
    {
        final Object character = ValueTraits.toScmChar('a');

        commonTests(character);
        assertTrue(ValueTraits.isScmChar(character));
        assertTrue(ValueTraits.toScmChar(character) == character);
    }

    public void testString()
        throws Exception
    {
        final Object string = ScmString.create("Hallo !");

        commonTests(string);
        assertTrue(ValueTraits.isScmString(string));
        assertTrue(ValueTraits.toScmString(string) == string);
    }

    public void testVector()
        throws Exception
    {
        final Object vector = ScmVector.create();

        commonTests(vector);
        assertTrue(ValueTraits.isScmVector(vector));
        assertTrue(ValueTraits.toScmVector(vector) == vector);
    }

    public void testOutputPort()
        throws Exception
    {
        final Object port = OutputPort.create(new StringWriter());

        commonTests(port);
        assertTrue(ValueTraits.isPort(port));
        assertTrue(ValueTraits.toOutputPort(port) == port);
    }

    public void testInputPort()
        throws Exception
    {
        final Object port = InputPort.create(new StringReader(""));

        commonTests(port);
        assertTrue(ValueTraits.isPort(port));
        assertTrue(ValueTraits.toInputPort(port) == port);
    }

    public void testEnvironment()
        throws Exception
    {
        final Object environment = Environment.getEmpty();

        assertTrue(ValueTraits.isTrue(environment));

        assertTrue(countTypes(environment) == 0);
        assertTrue(countCasts(environment) == 1);

        assertTrue(ValueTraits.toEnvironment(environment) == environment);
    }

    public void testStaticEnvironment()
        throws Exception
    {
        final Object environment = Environment.getEmpty().getStatic();

        assertTrue(ValueTraits.isTrue(environment));

        assertTrue(countTypes(environment) == 0);
        assertTrue(countCasts(environment) == 1);

        assertTrue(ValueTraits.toStaticEnvironment(environment) == environment);
    }


    private int eqHelper(Object fst, Object snd)
    {
        boolean eq    = ValueTraits.eq(fst, snd);
        boolean eqv   = ValueTraits.eqv  (fst, snd);
        boolean equal = ValueTraits.equal(fst, snd);

        // reflexivity
        assertTrue(ValueTraits.eq   (fst,fst));
        assertTrue(ValueTraits.eq   (snd, snd));
        assertTrue(ValueTraits.eqv  (fst, fst));
        assertTrue(ValueTraits.eqv  (snd, snd));
        assertTrue(ValueTraits.equal(fst, fst));
        assertTrue(ValueTraits.equal(snd, snd));

        // symmetry
        assertTrue(eq    == ValueTraits.eq   (snd, fst));
        assertTrue(eqv   == ValueTraits.eqv  (snd, fst));
        assertTrue(equal == ValueTraits.equal(snd, fst));


        assertTrue(!eq  | eqv  ); // aka. eq  -> eqv
        assertTrue(!eqv | equal); // aka. eqv -> equal

        if (eq)
        {
            return 3;
        }
        else if (eqv)
        {
            return 2;
        }
        else if (equal)
        {
            return 1;
        }
        else
        {
            return 0;
        }
    }

    public void testEq()
        throws Exception
    {
		Object u = Symbol.create("u");
		Object v = Symbol.create("v");

        // eq equivalent values

        assertTrue(
            eqHelper(v, v) == 3
        );

        assertTrue(
            eqHelper(
				ValueTraits.TRUE,
				ValueTraits.TRUE
            ) == 3
        );

        assertTrue(
            eqHelper(
				ValueTraits.FALSE,
				ValueTraits.FALSE
            ) == 3
        );

        assertTrue(
            eqHelper(
                Symbol.create("a"),
                Symbol.create("a")
            ) == 3
        );

        assertTrue(
            eqHelper(
                Empty.create(),
                Empty.create()
            ) == 3
        );


        // eqv equivalent values

        assertTrue(
            eqHelper(
                ScmNumber.create(7123645),
                ScmNumber.create(7123645)
            ) >= 2
        );

        assertTrue(
            eqHelper(
                ValueTraits.toScmChar('u'),
				ValueTraits.toScmChar('u')
            ) >= 2
        );


        // equal equivalent but eqv unspec. values

        assertTrue(
            eqHelper(
                ScmVector.create(),
                ScmVector.create()
            ) >= 1
        );

        assertTrue(
            eqHelper(
                ScmVector.create(5, v),
                ScmVector.create(5, v)
            ) >= 1
        );

        assertTrue(
            eqHelper(
                ScmString.create(""),
                ScmString.create("")
            ) >= 1
        );

        assertTrue(
            eqHelper(
                ScmString.create("Hallo"),
                ScmString.create("Hallo")
            ) >= 1
        );


        // equal equivalent but eqv different values

        assertTrue(
            eqHelper(
                ListFactory.createPair(v, v),
                ListFactory.createPair(v, v)
            ) == 1
        );


        // different values

        assertTrue(
            eqHelper(
				ValueTraits.TRUE,
                ValueTraits.FALSE
            ) == 0
        );

        assertTrue(
            eqHelper(
                Symbol.create("u"),
                Symbol.create("v")
            ) == 0
        );

        assertTrue(
            eqHelper(
                ScmVector.create(5, u),
                ScmVector.create(5, v)
            ) == 0
        );

        assertTrue(
            eqHelper(
                ScmVector.create(7, v),
                ScmVector.create(5, v)
            ) == 0
        );

        assertTrue(
            eqHelper(
                ScmString.create("Hallo 1"),
                ScmString.create("Hallo 2")
            ) == 0
        );
    }
}
