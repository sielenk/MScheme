package MScheme;

import java.io.StringReader;
import junit.framework.*;

import MScheme.machine.Machine;
import MScheme.environment.*;
import MScheme.code.*;
import MScheme.syntax.SyntaxFactory;
import MScheme.values.*;
import MScheme.exceptions.*;


public class TestMachine
    extends TestCase
{
    private Machine machine;
    
    private Symbol      _sym1;
    private Symbol      _sym2;
    private Value       _val1;
    private Value       _val2;
    private Value       _unval;
    private DynamicEnvironment _environment;


    public TestMachine(String name)
    { super(name); }
    
    
    protected void setUp()
        throws Exception
    {
        _sym1 = ValueFactory.createSymbol("test1");
        _sym2 = ValueFactory.createSymbol("test2");
    
        _val1  = ValueFactory.createTrue();
        _val2  = ValueFactory.createFalse();
        _unval = ValueFactory.createList();
    
        _environment = DynamicEnvironment.getEmpty();
    
        StaticEnvironment staticBindings =
            _environment.getStatic();
        
        staticBindings.defineSyntax(
            ValueFactory.createSymbol("quote"),
            SyntaxFactory.getQuoteToken()
        );
        staticBindings.defineSyntax(
            ValueFactory.createSymbol("if"),
            SyntaxFactory.getIfToken()
        );
        staticBindings.defineSyntax(
            ValueFactory.createSymbol("begin"),
            SyntaxFactory.getBeginToken()
        );
        staticBindings.defineSyntax(
            ValueFactory.createSymbol("lambda"),
            SyntaxFactory.getLambdaToken()
        );
        staticBindings.defineSyntax(
            ValueFactory.createSymbol("define"),
            SyntaxFactory.getDefineToken()
        );
        staticBindings.defineSyntax(
            ValueFactory.createSymbol("set!"),
            SyntaxFactory.getSetToken()
        );

        machine = new Machine(_environment);
    }
    
    protected void tearDown()
    {
        machine = null;
        _environment = null;
    }


    private Value evaluate(String expression)
        throws SchemeException
    {
        return machine.evaluate(
            ValueFactory.createInputPort(
                new StringReader(expression)
            ).read()
        );
    }


    public void testTestValues()
        throws Exception
    {
        try {
            assert(_unval.getCode(_environment.getStatic()) != null);
            fail("expected CantCompileException");
        }
        catch (CantCompileException e) { }
        
        assert(_val1.getCode(_environment.getStatic()) != null);
        assert(_val2.getCode(_environment.getStatic()) != null);
    }
    
    public void testEnvironment()
    {
        assert(machine.getEnvironment() == _environment);
    
        DynamicEnvironment
            otherEnvironment = _environment.newChild();
        
        machine.setEnvironment(otherEnvironment);

        assert(machine.getEnvironment() == otherEnvironment);
    }

    public void testValue()
        throws Exception
    {
        assert(machine.evaluate(_val1) == _val1);
        assert(machine.evaluate(_val2) == _val2);
    }

    public void testUnevaluatable()
        throws Exception
    {
        try {
            machine.evaluate(_unval);
            fail("evaluated Unevaluatable");
        }
        catch (CantCompileException e) { }
    }
    
    private void define(Symbol s, Value v)
        throws Exception
    {
        _environment.define(s, v);
    }
    
    public void testSymbol()
        throws Exception
    {
        try {
            machine.evaluate(_sym1);
            fail("expected SymbolNotFoundException");
        }
        catch (SymbolNotFoundException e) { }
        
        define(_sym1, _val1);
        define(_sym2, _unval);
    
        assert(
            "evaluation to Value failed",
            machine.evaluate(_sym1) == _val1
        );
        assert(
            "evaluation to Unevaluatable failed",
            machine.evaluate(_sym2) == _unval
        );
    }
    
    public void testPair()
        throws Exception
    {
        try {
            assert(
                machine.evaluate(
                    Pair.create(_val1, _val2)
                ) != null
            );
            fail("expected ListExpectedException");
        }
        catch (ListExpectedException e) { }

        try {
            assert(
                machine.evaluate(
                    ValueFactory.createList(_val1)
                ) != null
            );
            fail("expected FunctionExpectedException");
        }
        catch (FunctionExpectedException e) { }
    }
    
    public void testQuote()
        throws Exception
    {
        assert(
            machine.evaluate(
                ValueFactory.createList(
                    ValueFactory.createSymbol("quote"),
                    _unval
                )
            ) == _unval
        );
    }
    
    public void testIf()
        throws Exception
    {
        define(_sym1, _val1);
        define(_sym2, _val2);
    
        assert(
            machine.evaluate(
                ValueFactory.prepend(
                    ValueFactory.createSymbol("if"),
                    ValueFactory.createList(
                        ValueFactory.createTrue(),
                        _sym1,
                        _sym2
                    )
                )
            ) == _val1
        );

        assert(
            machine.evaluate(
                ValueFactory.prepend(
                    ValueFactory.createSymbol("if"),
                    ValueFactory.createList(
                        ValueFactory.createTrue(),
                        _sym1
                    )
                )
            ) == _val1
        );

        assert(
            machine.evaluate(
                ValueFactory.prepend(
                    ValueFactory.createSymbol("if"),
                    ValueFactory.createList(
                        ValueFactory.createFalse(),
                        _sym1,
                        _sym2
                    )
                )
            ) == _val2
        );
    }   

    public void testBegin()
        throws Exception
    {
        define(_sym2, _val2);

        try {
            machine.evaluate(
                ValueFactory.createList(
                    ValueFactory.createSymbol("begin"),
                    _sym1,
                    _sym2
                )
            );
            fail("begin failed");
        }
        catch (SymbolNotFoundException e) { }

        _environment.define(_sym1, _val1);
        
        assert(
            machine.evaluate(
                ValueFactory.createList(
                    ValueFactory.createSymbol("begin"),
                    _sym1,
                    _sym2
                )
            ) == _val2
        );
    }
    
    public void testLambdaFailures()
        throws Exception
    {
        try {
            evaluate("(lambda () #(1 2 3))");
            fail("expected CantCompileException");
        }
        catch (CantCompileException e) { }
        
        try {
            evaluate("(lambda (#t) #t)");
            fail("expected SymbolExpectedException");
        }
        catch (SymbolExpectedException e) { }
        
        try {
            evaluate("(lambda (x y x) #t)");
            fail("expected DuplicateSymbolException");
        }
        catch (DuplicateSymbolException e) { }
    }
        
    public void testLambdaNoArgs()
        throws Exception
    {
        Function func = machine.evaluate(
            ValueFactory.createList(
                ValueFactory.createSymbol("lambda"),
                ValueFactory.createList(),
                _val1
            )
        ).toFunction();
        
        assert(
            machine.evaluate(ValueFactory.createList(func)) == _val1
        );

        try {
            machine.evaluate(ValueFactory.createList(func, _unval));
            fail("expected CantEvaluateException");
        }
        catch (CantCompileException e) { }
        
        try {
            machine.evaluate(ValueFactory.createList(func, _val1));
            fail("expected ArityException");
        }
        catch (ArityException e) { }
    }

    public void testLambdaWithSimpleArgs()
        throws Exception
    {
        Function func = evaluate("(lambda (x y) x)").toFunction();
        
        assert(
            machine.evaluate(
                ValueFactory.createList(
                    func,
                    _val1,
                    _val2
                )
            ) == _val1
        );
        
        try {
            machine.evaluate(ValueFactory.createList(func, _val1));
            fail("expected ArityException");
        }
        catch (ArityException e) { }
    }

    public void testLambdaWithOptionalArgs()
        throws Exception
    {
        Function func = evaluate("(lambda (x . y) y)").toFunction();
        
        try {
            machine.evaluate(ValueFactory.createList(func));
            fail("expected ArityException");
        }
        catch (ArityException e) { }
        
        assert(
            machine.evaluate(
                ValueFactory.createList(func, _val1)
            ) == ValueFactory.createList()
        );
        
        assert(
            machine.evaluate(
                ValueFactory.createList(func, _val1, _val2)
            ).toList().getHead() == _val2
        );
    }
    
    public void testLambdaOptionalIsNewList()
        throws Exception
    {
        Function func = evaluate("(lambda x x)").toFunction();

        Pair pair2 = Pair.create(
            _val2,
            ValueFactory.createList()
        );
        Pair pair1 = Pair.create(
            _val1,
            pair2
        );
    
        Value result = machine.evaluate(
            Pair.create(func, pair1)
        );

        assert(result.equal(pair1));
        assert(!result.eq(pair1));
    }
    
    public void testDefineNormal()
        throws Exception
    {
        machine.evaluate(
            ValueFactory.createList(
                ValueFactory.createSymbol("define"),
                ValueFactory.createSymbol("a"),
                _val1
            )
        );
        
        assert(
            machine.evaluate(
                ValueFactory.createSymbol("a")
            ) == _val1
        );
    }

    public void testApplication()
        throws Exception
    {
        evaluate("(define f (lambda (x) x))");

        assert(
            machine.evaluate(
                ValueFactory.createList(
                    ValueFactory.createSymbol("f"),
                    _val1
                )
            ) == _val1
        );
    }

    public void testDefineFunction()
        throws Exception
    {
        evaluate("(define (f x y) x)");
        evaluate("(define (g . x) x)");

        assert(
            "function creation failed",
            machine.evaluate(
                ValueFactory.createSymbol("f")
            ).isFunction()
        );
        
        assert(
            "function application failed",
            machine.evaluate(
                ValueFactory.createList(
                    ValueFactory.createSymbol("f"),
                    _val1,
                    _val2
                )
            ) == _val1
        );
    }
    
    public void testCallCC()
        throws Exception
    {
        assert(
            machine.evaluate(
                ValueFactory.createList(
                    ValueFactory.createFunction("CallCC"),
                    ValueFactory.createList(
                        ValueFactory.createSymbol("lambda"),
                        ValueFactory.createList(
                            ValueFactory.createSymbol("return")
                        ),
                        ValueFactory.createList(
                            ValueFactory.createSymbol("return"),
                            _val1
                        )
                    )
                )
            ) == _val1
        );
    }
}

