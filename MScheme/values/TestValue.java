package MScheme.values;

import MScheme.exceptions.*;
import MScheme.values.Value;


public class TestValue
    extends junit.framework.TestCase
{
    public TestValue(String name)
    { super(name); }
    
    protected void setUp()
        throws Exception
    {
    }
    
    protected void tearDown()
    {
    }
    

    private int countTypes(Value v)
    {
        int count = 0;
    
        if (v.isBoolean ()) count++;
        if (v.isPair    ()) count++;
        if (v.isSymbol  ()) count++;
        if (v.isNumber  ()) count++;
        if (v.isChar    ()) count++;
        if (v.isString  ()) count++;
        if (v.isVector  ()) count++;
        if (v.isPort    ()) count++;
        if (v.isFunction()) count++;
        
        return count;
    }
    
    private void checkNotAList(Value v)
    {
        assert(!v.isList());
    }


    public void testCastFunctions()
        throws Exception
    {
        ValueFactory.createPair(null, null).toPair();
        ValueFactory.createList().toList();
        ValueFactory.createNumber(0).toNumber();
        ValueFactory.createChar('a').toChar();
        ValueFactory.createString("").toScmString();
        ValueFactory.createVector().toVector();
        ValueFactory.createInputPort().toPort().toInput();
        ValueFactory.createOutputPort().toPort().toOutput();
    }

    public void testTrue()
        throws Exception
    {
        final Value True  = ValueFactory.createTrue();

        assert(!True.isFalse());
        
        assert(True.isBoolean());
        assert(countTypes(True) == 1);
        
        checkNotAList(True);
    }
    
    public void testFalse()
        throws Exception
    {
        final Value False = ValueFactory.createFalse();

        assert(False.isFalse());
        
        assert(False.isBoolean());
        assert(countTypes(False) == 1);
        
        checkNotAList(False);
    }
    
    public void testEmpty()
        throws Exception
    {
        final Value empty = ValueFactory.createList();
        
        assert(!empty.isFalse());
        
        assert(countTypes(empty) == 0);
        
        assert(empty.isList());
    
        assert(empty.toList() == empty);
    }
        
    public void testPair()
        throws Exception
    {       
        final Value pair = ValueFactory.createPair(
            ValueFactory.createTrue(),
            ValueFactory.createTrue()
        );
        
        assert(!pair.isFalse());
        
        assert(pair.isPair());
        assert(countTypes(pair) == 1);

        checkNotAList(pair);

        assert(pair.toPair() == pair);
    }
    
    public void testList()
        throws Exception
    {
        final Value list = ValueFactory.createList(
            ValueFactory.createTrue()
        );
        
        assert(!list.isFalse());
        
        assert(list.isPair());
        assert(countTypes(list) == 1);
        
        assert(list.isList());

        assert(list.toList() == list);
    }
    
    public void testSymbol()
        throws Exception
    {
        final Value symbol = ValueFactory.createSymbol("test");
        
        assert(!symbol.isFalse());

        assert(symbol.isSymbol());
        assert(countTypes(symbol) == 1);
        
        checkNotAList(symbol);
        
        assert(symbol.toSymbol() == symbol);
    }

    public void testFunction()
        throws Exception
    {
        final Value function = ValueFactory.createFunction("CallCC");
        
        assert(!function.isFalse());
    
        assert(function.isFunction());
        assert(countTypes(function) == 1);
        
        checkNotAList(function);
        
        assert(function.toFunction() == function);
    }
    

    private void commonLiteralTests(Value literal)
        throws Exception
    {
        assert(!literal.isFalse());
        assert(countTypes(literal) == 1);
        checkNotAList(literal);
    }
    
    public void testNumber()
        throws Exception
    {
        final Value number = ValueFactory.createNumber(49875);
    
        commonLiteralTests(number);
        assert(number.isNumber());
    }
    
    public void testChar()
        throws Exception
    {
        final Value character = ValueFactory.createChar('a');
    
        commonLiteralTests(character);
        assert(character.isChar());
    }
    
    public void testString()
        throws Exception
    {
        final Value string = ValueFactory.createString("Hallo !");
    
        commonLiteralTests(string);
        assert(string.isString());
    }
    
    public void testVector()
        throws Exception
    {
        final Value vector = ValueFactory.createVector();
    
        commonLiteralTests(vector);
        assert(
            vector.isVector()
        );
    }
    
    public void testPort()
        throws Exception
    {
        final Value portI = ValueFactory.createInputPort();
        final Value portO = ValueFactory.createOutputPort();
    
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
        Value u = ValueFactory.createSymbol("u");
        Value v = ValueFactory.createSymbol("v");
        
        // eq equivalent values
        
        assert(
            eqHelper(v, v) == 3
        );
        
        assert(
            eqHelper(
                ValueFactory.createTrue(),
                ValueFactory.createTrue()
            ) == 3
        );
        
        assert(
            eqHelper(
                ValueFactory.createFalse(),
                ValueFactory.createFalse()
            ) == 3
        );
        
        assert(
            eqHelper(
                ValueFactory.createSymbol("a"),
                ValueFactory.createSymbol("a")
            ) == 3
        );
        
        assert(
            eqHelper(
                ValueFactory.createList(),
                ValueFactory.createList()
            ) == 3
        );
        
        assert(
            eqHelper(
                ValueFactory.createFunction("CallCC"),
                ValueFactory.createFunction("CallCC")
            ) == 3
        );


        // eqv equivalent values
    
        assert(
            eqHelper(
                ValueFactory.createNumber(7123645),
                ValueFactory.createNumber(7123645)
            ) >= 2
        );
        
        assert(
            eqHelper(
                ValueFactory.createChar('u'),
                ValueFactory.createChar('u')
            ) >= 2
        );


        // equal equivalent but eqv unspec. values
        
        assert(
            eqHelper(
                ValueFactory.createVector(),
                ValueFactory.createVector()
            ) >= 1
        );
        
        assert(
            eqHelper(
                ValueFactory.createVector(5, v),
                ValueFactory.createVector(5, v)
            ) >= 1
        );

        assert(
            eqHelper(
                ValueFactory.createString(""),
                ValueFactory.createString("")
            ) >= 1
        );

        assert(
            eqHelper(
                ValueFactory.createString("Hallo"),
                ValueFactory.createString("Hallo")
            ) >= 1
        );
        

        // equal equivalent but eqv different values

        assert(
            eqHelper(
                ValueFactory.createPair(v, v),
                ValueFactory.createPair(v, v)
            ) == 1
        );
        

        // different values

        assert(
            eqHelper(
                ValueFactory.createTrue(),
                ValueFactory.createFalse()
            ) == 0
        );
        
        assert(
            eqHelper(
                ValueFactory.createSymbol("u"),
                ValueFactory.createSymbol("v")
            ) == 0
        );

        assert(
            eqHelper(
                ValueFactory.createVector(5, u),
                ValueFactory.createVector(5, v)
            ) == 0
        );

        assert(
            eqHelper(
                ValueFactory.createVector(7, v),
                ValueFactory.createVector(5, v)
            ) == 0
        );

        assert(
            eqHelper(
                ValueFactory.createString("Hallo 1"),
                ValueFactory.createString("Hallo 2")
            ) == 0
        );
    }
}

