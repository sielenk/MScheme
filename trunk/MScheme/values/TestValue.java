package MScheme.values;

import MScheme.environment.StaticEnvironment;
import MScheme.environment.Environment;

import MScheme.exceptions.*;
import MScheme.Value;

import MScheme.functions.CallCCFunction;


public class TestValue
    extends junit.framework.TestCase
{
    public final static String id
        = "$Id$";


    public TestValue(String name)
    { super(name); }

    protected void setUp()
        throws Exception
    { }

    protected void tearDown()
    { }


    private int countTypes(Value v)
    {
        int count = 0;

        if (v.isScmBoolean()) count++;
        if (v.isPair      ()) count++;
        if (v.isSymbol    ()) count++;
        if (v.isScmNumber ()) count++;
        if (v.isScmChar   ()) count++;
        if (v.isScmString ()) count++;
        if (v.isScmVector ()) count++;
        if (v.isPort      ()) count++;
        if (v.isFunction  ()) count++;

        return count;
    }

    private void checkNotAList(Value v)
    {
        assert(!v.isList());
    }


    public void testCastFunctions()
        throws Exception
    {
        Pair      .create(null, null).toPair();
        Empty     .create().toList();
        ScmNumber .create(0).toScmNumber();
        ScmChar   .create('a').toScmChar();
        ScmString .create("").toScmString();
        ScmVector .create().toScmVector();
        InputPort .create().toInputPort();
        OutputPort.create().toOutputPort();

        {
            Environment empty = Environment.getEmpty();

            empty.toEnvironment();
            empty.getStatic().toStaticEnvironment();
        }
    }

    public void testTrue()
        throws Exception
    {
        final Value True  = ScmBoolean.createTrue();

        assert(True.isTrue());

        assert(True.isScmBoolean());
        assert(countTypes(True) == 1);

        checkNotAList(True);
    }

    public void testFalse()
        throws Exception
    {
        final Value False = ScmBoolean.createFalse();

        assert(!False.isTrue());

        assert(False.isScmBoolean());
        assert(countTypes(False) == 1);

        checkNotAList(False);
    }

    public void testEmpty()
        throws Exception
    {
        final Value empty = Empty.create();

        assert(empty.isTrue());

        assert(countTypes(empty) == 0);

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

        assert(pair.isTrue());

        assert(pair.isPair());
        assert(countTypes(pair) == 1);

        checkNotAList(pair);

        assert(pair.toPair() == pair);
    }

    public void testList()
        throws Exception
    {
        final Value list = ListFactory.create(
            ScmBoolean.createTrue()
        );

        assert(list.isTrue());

        assert(list.isPair());
        assert(countTypes(list) == 1);

        assert(list.isList());

        assert(list.toList() == list);
    }

    public void testSymbol()
        throws Exception
    {
        final Value symbol = Symbol.create("test");

        assert(symbol.isTrue());

        assert(symbol.isSymbol());
        assert(countTypes(symbol) == 1);

        checkNotAList(symbol);

        assert(symbol.toSymbol() == symbol);
    }

    public void testFunction()
        throws Exception
    {
        final Value function = CallCCFunction.INSTANCE;

        assert(function.isTrue());

        assert(function.isFunction());
        assert(countTypes(function) == 1);

        checkNotAList(function);

        assert(function.toFunction() == function);
    }


    private void commonLiteralTests(Value literal)
        throws Exception
    {
        assert(literal.isTrue());
        assert(countTypes(literal) == 1);
        checkNotAList(literal);
    }

    public void testNumber()
        throws Exception
    {
        final Value number = ScmNumber.create(49875);

        commonLiteralTests(number);
        assert(number.isScmNumber());
    }

    public void testChar()
        throws Exception
    {
        final Value character = ScmChar.create('a');

        commonLiteralTests(character);
        assert(character.isScmChar());
    }

    public void testString()
        throws Exception
    {
        final Value string = ScmString.create("Hallo !");

        commonLiteralTests(string);
        assert(string.isScmString());
    }

    public void testVector()
        throws Exception
    {
        final Value vector = ScmVector.create();

        commonLiteralTests(vector);
        assert(
            vector.isScmVector()
        );
    }

    public void testPort()
        throws Exception
    {
        final Value portI = InputPort.create();
        final Value portO = OutputPort.create();

        commonLiteralTests(portI);
        assert(portI.isPort());

        commonLiteralTests(portO);
        assert(portO.isPort());
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
