package MScheme;

import java.io.StringReader;
import junit.framework.*;

import MScheme.machine.Machine;
import MScheme.environment.*;
import MScheme.code.*;
import MScheme.values.*;
import MScheme.exceptions.*;


public class TestR5RS
    extends TestCase
{
    private Machine machine;


    public TestR5RS(String name)
    { super(name); }


    protected void setUp()
        throws Exception
    {
        machine = new Machine();
    }

    protected void tearDown()
    {
        machine = null;
    }


    private Value quote(String expression)
        throws SchemeException
    {
        return ValueFactory.createInputPort(
                new StringReader(expression)
        ).read();
    }

    private Value eval(String expression)
        throws SchemeException
    {
        return machine.evaluate(
            quote(expression)
        );
    }


    public void test4_1_2()
        throws SchemeException
    {
        assert(eval("(quote a)"       ).equal(quote("a"       )));
        assert(eval("(quote #(a b c))").equal(quote("#(a b c)")));
        assert(eval("(quote (+ 1 2))" ).equal(quote("(+ 1 2)" )));

        assert(eval("'a"        ).equal(quote("a"        )));
        assert(eval("'#(a b c)" ).equal(quote("#(a b c)" )));
        assert(eval("'()"       ).equal(quote("()"       )));
        assert(eval("'(+ 1 2)"  ).equal(quote("(+ 1 2)"  )));
        assert(eval("'(quote a)").equal(quote("(quote a)")));
        assert(eval("''a"       ).equal(quote("(quote a)")));

        assert(eval("'\"abc\"").equal(quote("\"abc\"")));
        assert(eval("\"abc\"" ).equal(quote("\"abc\"")));
        assert(eval("'145932" ).equal(quote("145932" )));
        assert(eval("145932"  ).equal(quote("145932" )));
        assert(eval("'#t"     ).equal(quote("#t"     )));
        assert(eval("#t"      ).equal(quote("#t"     )));

        try {
            eval("'(1 . 2)").toPair().setFirst(quote("a"));
            fail();
        }
        catch (ImmutableException e) { }

        try {
            eval("'\"abc\"").toScmString().set(0, 'b');
            fail();
        }
        catch (ImmutableException e) { }

        try {
            eval("'#(1 2 3)").toVector().set(0, quote("a"));
            fail();
        }
        catch (ImmutableException e) { }
    }

    public void test4_1_3()
        throws SchemeException
    {
        assert(eval("(+ 3 4)"          ).equal(quote("7" )));
        assert(eval("((if #f + *) 3 4)").equal(quote("12")));

        try {
            eval("()");
            fail();
        }
        catch (SyntaxException e) { }
    }
}

