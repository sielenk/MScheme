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
        return InputPort.create(
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
    {
        Value   value   = eval(in);
        boolean success = value.equal(quote(out));

        if (!success) {
            System.out.println(
                "*** evaluation of ***\n" +
                in + '\n' +
                "*** returned ***\n" +
                value + '\n' +
                "*** expected was ***\n" +
                out + '\n' +
                "*** end ***"
            );
        }

        assert(success);
    }


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
            eval("'#(1 2 3)").toScmVector().set(0, quote("a"));
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
        catch (CompileError e) { }
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
        catch (CompileError e) { }

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
    public void no_test4_2_1()
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
    public void test4_2_2_let()
        throws SchemeException
    {
        check(
            "(let ((x 2) (y 3))\n" +
            "  (* x y))",
            "6"
        );

        check(
            "(let ((x 2) (y 3))\n" +
            "  (let ((x 7)\n" +
            "        (z (+ x y)))\n" +
            "    (* z x)))",
            "35"
        );
    }

    public void test4_2_2_letstar()
        throws SchemeException
    {
        check(
            "(let ((x 2) (y 3))\n" +
            "  (let* ((x 7)\n" +
            "         (z (+ x y)))\n" +
            "    (* z x)))",
            "70"
        );

        check(
            "(let* ((a 1) (b (+ a 1)) (c (+ a b)) (d (+ b c)) (e (+ c d)))" +
            "  (list a b c d e))",
            "(1 2 3 5 8)"
        );
    }

    public void test4_2_2_letrec()
        throws SchemeException
    {
        check(
            "(letrec ((even?\n" +
            "          (lambda (n)\n" +
            "            (if (zero? n)\n" +
            "                #t\n" +
            "                (odd? (- n 1)))))\n" +
            "         (odd?\n" +
            "          (lambda (n)\n" +
            "            (if (zero? n)\n" +
            "                #f\n" +
            "                (even? (- n 1))))))\n" +
            "  (even? 88))\n",
            "#t"
        );
    }

    public void test4_2_2_common()
        throws SchemeException
    {
        check("(let ((x 1)) (let    () (define x 2)) x)", "1");
        check("(let ((x 1)) (let*   () (define x 2)) x)", "1");
        check("(let ((x 1)) (letrec () (define x 2)) x)", "1");
        
        check("(let ((x 1)) (begin     (define x 2)) x)", "2");
    }

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
    public void test4_2_6()
        throws SchemeException
    {
        check("`(list ,(+ 1 2) 4)", "(list 3 4)");
        check(
            "(let ((name 'a)) `(list ,name ',name))", 
            "(list a (quote a))"
        );
        check(
            "`(a ,(+ 1 2) ,@(list 4 5 6) b)",
            "(a 3 4 5 6 b)"
        );
        check(
            "`#(10 5 ,(+ 1 1) ,@(list 4 3) 8)",
            "#(10 5 2 4 3 8)"
        );
        check(
            "`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)",
            "(a `(b ,(+ 1 2) ,(foo 4 d) e) f)"
        );
        check(
            "(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))",
            "(a `(b ,x ,'y d) e))"
        );
    }

    /// 6.1 Equivalence predicates
    public void test6_1()
        throws SchemeException
    {
        // eqv?
        check("(eqv? 'a 'a)", "#t");
        check("(eqv? 'a 'b)", "#f");
        check("(eqv? 2 2)", "#t");
        check("(eqv? '() '())", "#t");
        check("(eqv? (cons 1 2) (cons 1 2))", "#f");
        check("(eqv? (lambda () 1) (lambda () 2))", "#f");
        check("(eqv? #f 'nil)", "#f");
        check("(let ((p (lambda (x) x))) (eqv? p p))", "#t");
    }


    // 6.3 Other data types

    // 6.3.1 Booleans

    /// 6.3.2 Pairs and lists
    public void test6_3_2()
        throws SchemeException
    {
        // pair?
        check("(pair? '(a . b))", "#t");
        check("(pair? '(a b c))", "#t");
        check("(pair? '())", "#f");
        check("(pair? '#(a b))", "#f");

        // cons
        check("(cons 'a '())", "(a)");
        check("(cons '(a) '(b c d))", "((a) b c d)");
        check("(cons \"a\" '(b c))", "(\"a\" b c)");
        check("(cons 'a '3)", "(a . 3)");
        check("(cons '(a b) 'c)", "((a b) . c)");

        // car
        check("(car '(a b c))", "a");
        check("(car '((a) b c d))", "(a)");
        check("(car '(1 . 2))", "1");
        try { eval("(car '())"); fail(); }
        catch (SchemeException e) { }

        // cdr
        check("(cdr '((a) b c d))", "(b c d)");
        check("(cdr '(1 . 2))", "2");
        try { eval("(cdr '())"); fail(); }
        catch (SchemeException e) { }

        // set-car!
        eval("(define (f) (list 'not-a-constant-list))");
        eval("(define (g) '(constant-list))");
        eval("(set-car! (f) 3)");
        try { eval("(set-car! (g) 3)"); fail(); }
        catch (SchemeException e) { }

        // set-cdr!
        eval("(set-cdr! (f) 3)");
        try { eval("(set-cdr! (g) 3)"); fail(); }
        catch (SchemeException e) { }

        // list?
        check("(list? '(a b c))", "#t");
        check("(list? '())", "#t");
        check("(list? '(a . b))", "#f");

        // list
        check("(list 'a (+ 3 4) 'c)", "(a 7 c)");
        check("(list)", "()");

        // length
        check("(length '(a b c))", "3");
        check("(length '(a (b) (c d e)))", "3");
        check("(length '())", "0");

        // append
        check("(append '(x) '(y))", "(x y)");
        check("(append '(a) '(b c d))", "(a b c d)");    
        check("(append '(a (b)) '((c)))", "(a (b) (c))");

        check("(append '(a b) '(c . d))", "(a b c . d)");
        check("(append '() 'a)", "a");

        // reverse
        check("(reverse '(a b c))", "(c b a)");
        check("(reverse '(a (b c) d (e (f))))", "((e (f)) d (b c) a)");

        // memq/memv/member
        check("(memq 'a '(a b c))", "(a b c)");
        check("(memq 'b '(a b c))", "(b c)");
        check("(memq 'a '(b c d))", "#f");
        check("(memq   (list 'a) '(b (a) c))", "#f");
        check("(member (list 'a) '(b (a) c))", "((a) c)");
        check("(memv '101 '(100 101 102))", "(101 102)");

        // assq/assv/assoc
        eval("(define e '((a 1) (b 2) (c 3)))");
        check("(assq 'a e)", "(a 1)");
        check("(assq 'b e)", "(b 2)");
        check("(assq 'd e)", "#f");
        check("(assq  (list 'a) '(((a)) ((b)) ((c))))", "#f");
        check("(assoc (list 'a) '(((a)) ((b)) ((c))))", "((a))");
        check("(assv 5 '((2 3) (5 7) (11 13)))", "(5 7)");
    }

    /// 6.3.3 Symbols
    public void test6_3_3()
        throws SchemeException
    {
        // symbol?
        check("(symbol? 'foo)"        , "#t");
        check("(symbol? (car '(a b)))", "#t");
        check("(symbol? \"bar\")"     , "#f");
        check("(symbol? 'nil)"        , "#t");
        check("(symbol? '())"         , "#f");
        check("(symbol? #f)"          , "#f");

        // symbol->string
        // string->symbol
        check("(symbol->string 'flying-fish)", "\"flying-fish\"");
        check("(symbol->string 'Martin)"     , "\"martin\"");
        check("(symbol->string (string->symbol \"Malvia\"))", "\"Malvia\"");
    }
    

    /// 6.4 Control features
    public void test6_4()
        throws SchemeException
    {
        check("(procedure?  car)", "#t");
        check("(procedure? 'car)", "#f");
        check("(procedure?  (lambda (x) (* x x)))", "#t");
        check("(procedure? '(lambda (x) (* x x)))", "#f");
        check("(call-with-current-continuation procedure?)", "#t");

        check("(apply + (list 3 4))", "7");

        check(
            "(let ((path '())\n"+
            "      (c #f))\n"+
            "  (let ((add (lambda (s)\n"+
            "               (set! path (cons s path)))))\n"+
            "    (dynamic-wind\n"+
            "      (lambda () (add 'connect))\n"+
            "      (lambda ()\n"+
            "        (add (call-with-current-continuation\n"+
            "               (lambda (c0)\n"+
            "                 (set! c c0)\n"+
            "                 'talk1))))\n"+
            "      (lambda () (add 'disconnect)))\n"+
            "    (if (< (length path) 4)\n"+
            "        (c 'talk2)\n"+
            "        (reverse path))))",
            "(connect talk1 disconnect\n" +
            " connect talk2 disconnect)"
        );
    }


    /// 6.5 Eval
    public void test6_5()
        throws SchemeException
    {
        check("(eval '(* 7 3) (scheme-report-environment 5))", "21");
        check(
            "(let ((f (eval '(lambda (f x) (f x x))\n"+
            "       (null-environment 5))))\n"+
            "  (f + 10))",
            "20"
        );
    }
}

