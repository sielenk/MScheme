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

    private void check(String in, String out)
        throws SchemeException
    { assert(eval(in).equal(quote(out))); }


    // 4 Expressions

    // 4.1 Primitive expression types
    
    /// 4.1.1 Variable references
    public void test4_1_1()
        throws SchemeException
    {
        eval("(define x 28)");
        check("x", "28");
    }

    /// 4.1.2 Literal expressions
    public void test4_1_2()
        throws SchemeException
    {
        check("(quote a)"       , "a"       );
        check("(quote #(a b c))", "#(a b c)");
        check("(quote (+ 1 2))" , "(+ 1 2)" );

        check("'a"        , "a"        );
        check("'#(a b c)" , "#(a b c)" );
        check("'()"       , "()"       );
        check("'(+ 1 2)"  , "(+ 1 2)"  );
        check("'(quote a)", "(quote a)");
        check("''a"       , "(quote a)");

        check("'\"abc\"", "\"abc\"");
        check("\"abc\"" , "\"abc\"");
        check("'145932" , "145932" );
        check("145932"  , "145932" );
        check("'#t"     , "#t"     );
        check("#t"      , "#t"     );

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

    /// 4.1.3 Procedure calls
    public void test4_1_3()
        throws SchemeException
    {
        check("(+ 3 4)"          , "7" );
        check("((if #f + *) 3 4)", "12");

        try {
            eval("()");
            fail();
        }
        catch (SyntaxException e) { }
    }

    /// 4.1.4 Procedures
    public void test4_1_4()
        throws SchemeException
    {
        assert(eval("(lambda (x) (+ x x))").isFunction());
        check(   "((lambda (x) (+ x x)) 4)", "8");

        eval(
            "(define reverse-subtract\n" +
            "  (lambda (x y) (- y x)))"
        );
        check("(reverse-subtract 7 10)", "3");

        eval(
            "(define add4\n" +
            "  (let ((x 4))\n" +
            "    (lambda (y) (+ x y))))"
        );
        check("(add4 6)", "10");

        try {
            eval("(lambda (x y x) y)");
            fail();
        }
        catch (SyntaxException e) { }

        check("((lambda x x) 3 4 5 6)", "(3 4 5 6)");
        check("((lambda (x y .z) z) 3 4 5 6)", "(5 6)");
    }

    /// 4.1.5 Conditionals
    public void test4_1_5()
        throws SchemeException
    {
        check("(if (> 3 2) 'yes 'no)", "yes");
        check("(if (> 2 3) 'yes 'no)", "no");
        check("(if (> 3 2)\n" +
              "    (- 3 2)\n" +
              "    (+ 3 2))",
              "1"
        );
    }

    /// 4.1.6 Assignments
    public void test4_1_6()
        throws SchemeException
    {
        eval("(define x 2)");
        check("(+ x 1)", "3");
        eval("(set! x 4)");
        check("(+ x 1)", "5");
    }


    // 4.2 Derived expression types

    /// 4.2.1 Conditionals
    public void test4_2_1()
        throws SchemeException
    {
        check(
            "(cond ((> 3 2) 'greater)\n" +
            "      ((< 3 2) 'less))",
            "greater"
        );
        check(
            "(cond ((> 3 3) 'greater)\n" +
            "      ((< 3 3) 'less))\n" +
            "      (else 'equal))",
            "equal"
        );
        check(
            "(cond ((assv 'b '((a 1) (b 2))) => cadr)\n" +
            "      (else #f))",
            "2"
        );
    }

    /// 4.2.2 Binding constructs

    /// 4.2.3 Sequencing
    public void test4_2_3()
        throws SchemeException
    {
        eval("(define x 0)");
        check(
            "(begin (set! x 5)\n" +
            "       (+ x 1))",
            "6"
        );
    }

    /// 4.2.4 Iteration

    /// 4.2.5 Delayed evaluation

    /// 4.2.6 Quasiquotation

}

