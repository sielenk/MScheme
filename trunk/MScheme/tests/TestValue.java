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

package MScheme.tests;

import java.io.StringReader;
import java.io.StringWriter;

import MScheme.environment.StaticEnvironment;
import MScheme.environment.Environment;

import MScheme.exceptions.*;
import MScheme.Value;

import MScheme.values.*;

import MScheme.values.functions.CallCCFunction;


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


    private int countTypes(Value v)
    {
        int count = 0;

        if (v.isList      ()) ++count;
        if (v.isEmpty     ()) ++count;

        if (v.isScmBoolean()) ++count;
        if (v.isPair      ()) ++count;
        if (v.isSymbol    ()) ++count;
        if (v.isScmNumber ()) ++count;
        if (v.isScmChar   ()) ++count;
        if (v.isScmString ()) ++count;
        if (v.isScmVector ()) ++count;
        if (v.isPort      ()) ++count;
        if (v.isFunction  ()) ++count;

        return count;
    }

    private int countCasts(Value v)
    {
        int count = 0;

        try { v.toList             (); ++count; } catch (TypeError e) { }

        try { v.toPair             (); ++count; } catch (TypeError e) { }
        try { v.toSymbol           (); ++count; } catch (TypeError e) { }
        try { v.toScmNumber        (); ++count; } catch (TypeError e) { }
        try { v.toScmChar          (); ++count; } catch (TypeError e) { }
        try { v.toScmString        (); ++count; } catch (TypeError e) { }
        try { v.toScmVector        (); ++count; } catch (TypeError e) { }
        try { v.toInputPort        (); ++count; } catch (TypeError e) { }
        try { v.toOutputPort       (); ++count; } catch (TypeError e) { }
        try { v.toFunction         (); ++count; } catch (TypeError e) { }

        try { v.toEnvironment      (); ++count; } catch (TypeError e) { }
        try { v.toStaticEnvironment(); ++count; } catch (TypeError e) { }

        return count;
    }

    private void commonTests(Value v)
    {
        assertTrue(v.isTrue());

        assertTrue(countTypes(v) == 1);
        assertTrue(countCasts(v) == 1);
    }


    public void testFalse()
    throws Exception
    {
        final Value False = ScmBoolean.createFalse();

        assertTrue(!False.isTrue());

        assertTrue(countTypes(False) == 1);
        assertTrue(countCasts(False) == 0);

        assertTrue(False.isScmBoolean());
    }

    public void testTrue()
    throws Exception
    {
        final Value True  = ScmBoolean.createTrue();

        assertTrue(True.isTrue());

        assertTrue(countTypes(True) == 1);
        assertTrue(countCasts(True) == 0);

        assertTrue(True.isScmBoolean());
    }

    public void testEmpty()
    throws Exception
    {
        final Value empty = Empty.create();

        assertTrue(empty.isTrue());

        assertTrue(countTypes(empty) == 2);
        assertTrue(countCasts(empty) == 1);

        assertTrue(empty.isEmpty());

        assertTrue(empty.isList());
        assertTrue(empty.toList() == empty);
    }

    public void testPair()
    throws Exception
    {
        final Value pair = ListFactory.createPair(
                               ScmBoolean.createTrue(),
                               ScmBoolean.createTrue()
                           );

        commonTests(pair);
        assertTrue(pair.isPair());
        assertTrue(pair.toPair() == pair);
    }

    public void testList()
    throws Exception
    {
        final Value list = ListFactory.create(
                               ScmBoolean.createTrue()
                           );

        assertTrue(list.isTrue());

        assertTrue(countTypes(list) == 2);
        assertTrue(countCasts(list) == 2);

        assertTrue(list.isPair());
        assertTrue(list.toPair() == list);

        assertTrue(list.isList());
        assertTrue(list.toList() == list);
    }

    public void testSymbol()
    throws Exception
    {
        final Value symbol = Symbol.create("test");

        commonTests(symbol);
        assertTrue(symbol.isSymbol());
        assertTrue(symbol.toSymbol() == symbol);
    }

    public void testFunction()
    throws Exception
    {
        final Value function = CallCCFunction.INSTANCE;

        commonTests(function);
        assertTrue(function.isFunction());
        assertTrue(function.toFunction() == function);
    }

    public void testNumber()
    throws Exception
    {
        final Value number = ScmNumber.create(49875);

        commonTests(number);
        assertTrue(number.isScmNumber());
        assertTrue(number.toScmNumber() == number);
    }

    public void testChar()
    throws Exception
    {
        final Value character = ScmChar.create('a');

        commonTests(character);
        assertTrue(character.isScmChar());
        assertTrue(character.toScmChar() == character);
    }

    public void testString()
    throws Exception
    {
        final Value string = ScmString.create("Hallo !");

        commonTests(string);
        assertTrue(string.isScmString());
        assertTrue(string.toScmString() == string);
    }

    public void testVector()
    throws Exception
    {
        final Value vector = ScmVector.create();

        commonTests(vector);
        assertTrue(vector.isScmVector());
        assertTrue(vector.toScmVector() == vector);
    }

    public void testOutputPort()
    throws Exception
    {
        final Value port = OutputPort.create(new StringWriter());

        commonTests(port);
        assertTrue(port.isPort());
        assertTrue(port.toOutputPort() == port);
    }

    public void testInputPort()
    throws Exception
    {
        final Value port = InputPort.create(new StringReader(""));

        commonTests(port);
        assertTrue(port.isPort());
        assertTrue(port.toInputPort() == port);
    }

    public void testEnvironment()
    throws Exception
    {
        final Value environment = Environment.getEmpty();

        assertTrue(environment.isTrue());

        assertTrue(countTypes(environment) == 0);
        assertTrue(countCasts(environment) == 1);

        assertTrue(environment.toEnvironment() == environment);
    }

    public void testStaticEnvironment()
    throws Exception
    {
        final Value environment = Environment.getEmpty().getStatic();

        assertTrue(environment.isTrue());

        assertTrue(countTypes(environment) == 0);
        assertTrue(countCasts(environment) == 1);

        assertTrue(environment.toStaticEnvironment() == environment);
    }


    private int eqHelper(Value fst, Value snd)
    {
        boolean eq    = fst.eq   (snd);
        boolean eqv   = fst.eqv  (snd);
        boolean equal = fst.equal(snd);

        // reflexivity
        assertTrue(fst.eq   (fst));
        assertTrue(snd.eq   (snd));
        assertTrue(fst.eqv  (fst));
        assertTrue(snd.eqv  (snd));
        assertTrue(fst.equal(fst));
        assertTrue(snd.equal(snd));

        // symmetry
        assertTrue(eq    == snd.eq   (fst));
        assertTrue(eqv   == snd.eqv  (fst));
        assertTrue(equal == snd.equal(fst));


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
        Value u = Symbol.create("u");
        Value v = Symbol.create("v");

        // eq equivalent values

        assertTrue(
            eqHelper(v, v) == 3
        );

        assertTrue(
            eqHelper(
                ScmBoolean.createTrue(),
                ScmBoolean.createTrue()
            ) == 3
        );

        assertTrue(
            eqHelper(
                ScmBoolean.createFalse(),
                ScmBoolean.createFalse()
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
                ScmChar.create('u'),
                ScmChar.create('u')
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
                ScmBoolean.createTrue(),
                ScmBoolean.createFalse()
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
