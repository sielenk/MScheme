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
import junit.framework.*;

import MScheme.Value;

import MScheme.machine.Machine;
import MScheme.environment.Environment;
import MScheme.syntax.SyntaxFactory;
import MScheme.values.*;
import MScheme.exceptions.*;


public class TestMachine
            extends TestCase
{
    public final static String id
        = "$Id$";

    private Machine machine;

    private Symbol      _sym1;
    private Symbol      _sym2;
    private Value       _val1;
    private Value       _val2;
    private Value       _unval;
    private Environment _environment;


    public TestMachine(String name)
    {
        super(name);
    }


    protected void setUp()
    throws Exception
    {
        _sym1 = Symbol.create("test1");
        _sym2 = Symbol.create("test2");

        _val1  = ScmBoolean.createTrue();
        _val2  = ScmBoolean.createFalse();
        _unval = Empty.create();

        _environment = Environment.getNullEnvironment();

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
                   InputPort.create(
                       new StringReader(expression)
                   ).read()
               );
    }


    public void testTestValues()
    throws Exception
    {
        try
        {
            assertTrue(_unval.getCode(_environment.getStatic()) != null);
            fail("expected CantCompileException");
        }
        catch (CantCompileException e)
        { }

        assertTrue(_val1.getCode(_environment.getStatic()) != null);
        assertTrue(_val2.getCode(_environment.getStatic()) != null);
    }

    public void testEnvironment()
    {
        assertTrue(machine.getEnvironment() == _environment);
    }

    public void testValue()
    throws Exception
    {
        assertTrue(machine.evaluate(_val1) == _val1);
        assertTrue(machine.evaluate(_val2) == _val2);
    }

    public void testUnevaluatable()
    throws Exception
    {
        try
        {
            machine.evaluate(_unval);
            fail("evaluated Unevaluatable");
        }
        catch (CantCompileException e)
        { }
    }

    private void define(Symbol s, Value v)
    throws Exception
    {
        _environment.define(s, v);
    }

    public void testSymbol()
    throws Exception
    {
        try
        {
            machine.evaluate(_sym1);
            fail("expected SymbolNotFoundException");
        }
        catch (SchemeException e)
        { }

        define(_sym1, _val1);
        define(_sym2, _unval);

        assertTrue(
            "evaluation to Value failed",
            machine.evaluate(_sym1) == _val1
        );
        assertTrue(
            "evaluation to Unevaluatable failed",
            machine.evaluate(_sym2) == _unval
        );
    }

    public void testPair()
    throws Exception
    {
        try
        {
            assertTrue(
                machine.evaluate(
                    ListFactory.createPair(_val1, _val2)
                ) != null
            );
            fail("expected ListExpected");
        }
        catch (ListExpected e)
        { }

        try
        {
            assertTrue(
                machine.evaluate(
                    ListFactory.create(_val1)
                ) != null
            );
            fail("expected FunctionExpected");
        }
        catch (FunctionExpected e)
        { }
    }

    public void testQuote()
    throws Exception
    {
        assertTrue(
            machine.evaluate(
                ListFactory.create(
                    Symbol.create("quote"),
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

        assertTrue(
            machine.evaluate(
                ListFactory.prepend(
                    Symbol.create("if"),
                    ListFactory.create(
                        ScmBoolean.createTrue(),
                        _sym1,
                        _sym2
                    )
                )
            ) == _val1
        );

        assertTrue(
            machine.evaluate(
                ListFactory.prepend(
                    Symbol.create("if"),
                    ListFactory.create(
                        ScmBoolean.createTrue(),
                        _sym1
                    )
                )
            ) == _val1
        );

        assertTrue(
            machine.evaluate(
                ListFactory.prepend(
                    Symbol.create("if"),
                    ListFactory.create(
                        ScmBoolean.createFalse(),
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

        try
        {
            machine.evaluate(
                ListFactory.create(
                    Symbol.create("begin"),
                    _sym1,
                    _sym2
                )
            );
            fail("begin failed");
        }
        catch (SchemeException e)
        { }

        _environment.define(_sym1, _val1);

        assertTrue(
            machine.evaluate(
                ListFactory.create(
                    Symbol.create("begin"),
                    _sym1,
                    _sym2
                )
            ) == _val2
        );
    }

    public void testLambdaFailures()
    throws Exception
    {
        try
        {
            evaluate("(lambda () #(1 2 3))");
            fail("expected CantCompileException");
        }
        catch (CantCompileException e)
        { }

        try
        {
            evaluate("(lambda (#t) #t)");
            fail("expected SymbolExpected");
        }
        catch (SymbolExpected e)
        { }

        try
        {
            evaluate("(lambda (x y x) #t)");
            fail("expected DuplicateSymbolException");
        }
        catch (DuplicateSymbolException e)
        { }
    }

    public void testLambdaNoArgs()
    throws Exception
    {
        Function func = machine.evaluate(
                            ListFactory.create(
                                Symbol.create("lambda"),
                                Empty.create(),
                                _val1
                            )
                        ).toFunction();

        assertTrue(
            machine.evaluate(ListFactory.create(func)) == _val1
        );

        try
        {
            machine.evaluate(ListFactory.create(func, _unval));
            fail("expected CantEvaluateException");
        }
        catch (CantCompileException e)
        { }

        try
        {
            machine.evaluate(ListFactory.create(func, _val1));
            fail("expected RuntimeArityError");
        }
        catch (RuntimeArityError e)
        { }
    }

    public void testLambdaWithSimpleArgs()
    throws Exception
    {
        Function func = evaluate("(lambda (x y) x)").toFunction();

        assertTrue(
            machine.evaluate(
                ListFactory.create(
                    func,
                    _val1,
                    _val2
                )
            ) == _val1
        );

        try
        {
            machine.evaluate(ListFactory.create(func, _val1));
            fail("expected RuntimeArityError");
        }
        catch (RuntimeArityError e)
        { }
    }

    public void testLambdaWithOptionalArgs()
    throws Exception
    {
        Function func = evaluate("(lambda (x . y) y)").toFunction();

        try
        {
            machine.evaluate(ListFactory.create(func));
            fail("expected RuntimeArityError");
        }
        catch (RuntimeArityError e)
        { }

        assertTrue(
            machine.evaluate(
                ListFactory.create(func, _val1)
            ) == Empty.create()
        );

        assertTrue(
            machine.evaluate(
                ListFactory.create(func, _val1, _val2)
            ).toList().getHead() == _val2
        );
    }

    public void testLambdaOptionalIsNewList()
    throws Exception
    {
        Function func = evaluate("(lambda x x)").toFunction();

        Pair pair2 = ListFactory.createPair(
                         _val2,
                         Empty.create()
                     );
        Pair pair1 = ListFactory.createPair(
                         _val1,
                         pair2
                     );

        Value result = machine.evaluate(
                           ListFactory.createPair(func, pair1)
                       );

        assertTrue(result.equal(pair1));
        assertTrue(!result.eq(pair1));
    }

    public void testDefineNormal()
    throws Exception
    {
        machine.evaluate(
            ListFactory.create(
                Symbol.create("define"),
                Symbol.create("a"),
                _val1
            )
        );

        assertTrue(
            machine.evaluate(
                Symbol.create("a")
            ) == _val1
        );
    }

    public void testApplication()
    throws Exception
    {
        evaluate("(define f (lambda (x) x))");

        assertTrue(
            machine.evaluate(
                ListFactory.create(
                    Symbol.create("f"),
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

        assertTrue(
            "function creation failed",
            machine.evaluate(
                Symbol.create("f")
            ).isFunction()
        );

        assertTrue(
            "function application failed",
            machine.evaluate(
                ListFactory.create(
                    Symbol.create("f"),
                    _val1,
                    _val2
                )
            ) == _val1
        );
    }

    public void testCallCC()
    throws Exception
    {
        assertTrue(
            machine.evaluate(
                ListFactory.create(
                    MScheme.values.functions.CallCCFunction.INSTANCE,
                    ListFactory.create(
                        Symbol.create("lambda"),
                        ListFactory.create(
                            Symbol.create("return")
                        ),
                        ListFactory.create(
                            Symbol.create("return"),
                            _val1
                        )
                    )
                )
            ) == _val1
        );
    }
}

