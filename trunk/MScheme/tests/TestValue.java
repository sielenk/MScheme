package MScheme.tests;

import MScheme.environment.StaticEnvironment;
import MScheme.environment.Environment;

import MScheme.exceptions.*;
import MScheme.Value;

import MScheme.values.*;

import MScheme.functions.CallCCFunction;


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
        assert(v.isTrue());

        assert(countTypes(v) == 1);
        assert(countCasts(v) == 1);
    }


    public void testFalse()
    throws Exception
    {
        final Value False = ScmBoolean.createFalse();

        assert(!False.isTrue());

        assert(countTypes(False) == 1);
        assert(countCasts(False) == 0);

        assert(False.isScmBoolean());
    }

    public void testTrue()
    throws Exception
    {
        final Value True  = ScmBoolean.createTrue();

        assert(True.isTrue());

        assert(countTypes(True) == 1);
        assert(countCasts(True) == 0);

        assert(True.isScmBoolean());
    }

    public void testEmpty()
    throws Exception
    {
        final Value empty = Empty.create();

        assert(empty.isTrue());

        assert(countTypes(empty) == 2);
        assert(countCasts(empty) == 1);

        assert(empty.isEmpty());

        assert(empty.isList());
        assert(empty.toList() == empty);
    }

    public void testPair()
    throws Exception
    {
        final Value pair = Pair.create(
                               ScmBoolean.createTrue(),
                               ScmBoolean.createTrue()
                           );

        commonTests(pair);
        assert(pair.isPair());
        assert(pair.toPair() == pair);
    }

    public void testList()
    throws Exception
    {
        final Value list = ListFactory.create(
                               ScmBoolean.createTrue()
                           );

        assert(list.isTrue());

        assert(countTypes(list) == 2);
        assert(countCasts(list) == 2);

        assert(list.isPair());
        assert(list.toPair() == list);

        assert(list.isList());
        assert(list.toList() == list);
    }

    public void testSymbol()
    throws Exception
    {
        final Value symbol = Symbol.create("test");

        commonTests(symbol);
        assert(symbol.isSymbol());
        assert(symbol.toSymbol() == symbol);
    }

    public void testFunction()
    throws Exception
    {
        final Value function = CallCCFunction.INSTANCE;

        commonTests(function);
        assert(function.isFunction());
        assert(function.toFunction() == function);
    }

    public void testNumber()
    throws Exception
    {
        final Value number = ScmNumber.create(49875);

        commonTests(number);
        assert(number.isScmNumber());
        assert(number.toScmNumber() == number);
    }

    public void testChar()
    throws Exception
    {
        final Value character = ScmChar.create('a');

        commonTests(character);
        assert(character.isScmChar());
        assert(character.toScmChar() == character);
    }

    public void testString()
    throws Exception
    {
        final Value string = ScmString.create("Hallo !");

        commonTests(string);
        assert(string.isScmString());
        assert(string.toScmString() == string);
    }

    public void testVector()
    throws Exception
    {
        final Value vector = ScmVector.create();

        commonTests(vector);
        assert(vector.isScmVector());
        assert(vector.toScmVector() == vector);
    }

    public void testOutputPort()
    throws Exception
    {
        final Value port = OutputPort.create();

        commonTests(port);
        assert(port.isPort());
        assert(port.toOutputPort() == port);
    }

    public void testInputPort()
    throws Exception
    {
        final Value port = InputPort.create();

        commonTests(port);
        assert(port.isPort());
        assert(port.toInputPort() == port);
    }

    public void testEnvironment()
    throws Exception
    {
        final Value environment = Environment.getEmpty();

        assert(environment.isTrue());

        assert(countTypes(environment) == 0);
        assert(countCasts(environment) == 1);

        assert(environment.toEnvironment() == environment);
    }

    public void testStaticEnvironment()
    throws Exception
    {
        final Value environment = Environment.getEmpty().getStatic();

        assert(environment.isTrue());

        assert(countTypes(environment) == 0);
        assert(countCasts(environment) == 1);

        assert(environment.toStaticEnvironment() == environment);
    }


    private int eqHelper(Value fst, Value snd)
    {
        boolean eq    = fst.eq   (snd);
        boolean eqv   = fst.eqv  (snd);
        boolean equal = fst.equal(snd);

        // reflexivity
        assert(fst.eq   (fst));
        assert(snd.eq   (snd));
        assert(fst.eqv  (fst));
        assert(snd.eqv  (snd));
        assert(fst.equal(fst));
        assert(snd.equal(snd));

        // symmetry
        assert(eq    == snd.eq   (fst));
        assert(eqv   == snd.eqv  (fst));
        assert(equal == snd.equal(fst));


        assert(!eq  | eqv  ); // aka. eq  -> eqv
        assert(!eqv | equal); // aka. eqv -> equal

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

        assert(
            eqHelper(v, v) == 3
        );

        assert(
            eqHelper(
                ScmBoolean.createTrue(),
                ScmBoolean.createTrue()
            ) == 3
        );

        assert(
            eqHelper(
                ScmBoolean.createFalse(),
                ScmBoolean.createFalse()
            ) == 3
        );

        assert(
            eqHelper(
                Symbol.create("a"),
                Symbol.create("a")
            ) == 3
        );

        assert(
            eqHelper(
                Empty.create(),
                Empty.create()
            ) == 3
        );


        // eqv equivalent values

        assert(
            eqHelper(
                ScmNumber.create(7123645),
                ScmNumber.create(7123645)
            ) >= 2
        );

        assert(
            eqHelper(
                ScmChar.create('u'),
                ScmChar.create('u')
            ) >= 2
        );


        // equal equivalent but eqv unspec. values

        assert(
            eqHelper(
                ScmVector.create(),
                ScmVector.create()
            ) >= 1
        );

        assert(
            eqHelper(
                ScmVector.create(5, v),
                ScmVector.create(5, v)
            ) >= 1
        );

        assert(
            eqHelper(
                ScmString.create(""),
                ScmString.create("")
            ) >= 1
        );

        assert(
            eqHelper(
                ScmString.create("Hallo"),
                ScmString.create("Hallo")
            ) >= 1
        );


        // equal equivalent but eqv different values

        assert(
            eqHelper(
                Pair.create(v, v),
                Pair.create(v, v)
            ) == 1
        );


        // different values

        assert(
            eqHelper(
                ScmBoolean.createTrue(),
                ScmBoolean.createFalse()
            ) == 0
        );

        assert(
            eqHelper(
                Symbol.create("u"),
                Symbol.create("v")
            ) == 0
        );

        assert(
            eqHelper(
                ScmVector.create(5, u),
                ScmVector.create(5, v)
            ) == 0
        );

        assert(
            eqHelper(
                ScmVector.create(7, v),
                ScmVector.create(5, v)
            ) == 0
        );

        assert(
            eqHelper(
                ScmString.create("Hallo 1"),
                ScmString.create("Hallo 2")
            ) == 0
        );
    }
}
