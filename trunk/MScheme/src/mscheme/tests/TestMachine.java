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

import junit.framework.TestCase;
import mscheme.compiler.Compiler;
import mscheme.environment.Environment;
import mscheme.exceptions.CantCompileException;
import mscheme.exceptions.CompileError;
import mscheme.exceptions.FunctionExpected;
import mscheme.exceptions.ListExpected;
import mscheme.exceptions.RuntimeArityError;
import mscheme.exceptions.SchemeException;
import mscheme.exceptions.SymbolExpected;
import mscheme.machine.Machine;
import mscheme.values.Function;
import mscheme.values.InputPort;
import mscheme.values.IList;
import mscheme.values.ListFactory;
import mscheme.values.IPair;
import mscheme.values.Symbol;
import mscheme.values.ValueTraits;


public class TestMachine extends TestCase
{
    public final static String CVS_ID = "$Id$";

    private Machine            machine;

    private Symbol             _sym1;
    private Symbol             _sym2;
    private Object             _val1;
    private Object             _val2;
    private Object             _unval;
    private Environment        _environment;

    public TestMachine(String name)
    {
        super(name);
    }

    protected void setUp() throws Exception
    {
        _sym1 = Symbol.create("test1");
        _sym2 = Symbol.create("test2");

        _val1  = Boolean.TRUE;
        _val2  = Boolean.FALSE;
        _unval = ListFactory.create();

        _environment = Environment.getNullEnvironment();

        machine = new Machine(_environment);
    }

    protected void tearDown()
    {
        machine = null;
        _environment = null;
    }

    private Object evaluate(String expression) throws SchemeException
    {
        return machine.evaluate(InputPort.create(new StringReader(expression))
                .read());
    }

    public void testTestValues() throws Exception
    {
        try
        {
            assertTrue(Compiler
                    .getForceable(_environment.getStatic(), _unval) != null);
            fail("expected CantCompileException");
        }
        catch (CantCompileException e)
        {}

        assertTrue(Compiler.getForceable(_environment.getStatic(), _val1) != null);
        assertTrue(Compiler.getForceable(_environment.getStatic(), _val2) != null);
    }

    public void testEnvironment()
    {
        assertTrue(machine.getEnvironment() == _environment);
    }

    public void testValue() throws Exception
    {
        assertTrue(machine.evaluate(_val1) == _val1);
        assertTrue(machine.evaluate(_val2) == _val2);
    }

    public void testUnevaluatable() throws Exception
    {
        try
        {
            machine.evaluate(_unval);
            fail("evaluated Unevaluatable");
        }
        catch (CantCompileException e)
        {}
    }

    private void define(Symbol s, Object v) throws Exception
    {
        _environment.define(s, v);
    }

    public void testSymbol() throws Exception
    {
        try
        {
            machine.evaluate(_sym1);
            fail("expected SymbolNotFoundException");
        }
        catch (SchemeException e)
        {}

        define(_sym1, _val1);
        define(_sym2, _unval);

        assertTrue("evaluation to Value failed",
            machine.evaluate(_sym1) == _val1);
        assertTrue("evaluation to Unevaluatable failed", machine
                .evaluate(_sym2) == _unval);
    }

    public void testPair1() throws Exception
    {
        try
        {
            assertTrue(machine.evaluate(ListFactory.createPair(_val1, _val2)) != null);
            fail("expected ListExpected");
        }
        catch (ListExpected e)
        {}
    }

    public void testPair2() throws Exception
    {
        try
        {
            assertTrue(machine.evaluate(ListFactory.create(_val1)) != null);
            fail("expected FunctionExpected");
        }
        catch (FunctionExpected e)
        {}
    }

    public void testQuote() throws Exception
    {
        assertTrue(machine.evaluate(ListFactory.create(Symbol.create("quote"),
            _unval)) == _unval);
    }

    public void testIf() throws Exception
    {
        define(_sym1, _val1);
        define(_sym2, _val2);

        assertTrue(machine.evaluate(ListFactory.prepend(Symbol.create("if"),
            ListFactory.create(Boolean.TRUE, _sym1, _sym2))) == _val1);

        assertTrue(machine.evaluate(ListFactory.prepend(Symbol.create("if"),
            ListFactory.create(Boolean.TRUE, _sym1))) == _val1);

        assertTrue(machine.evaluate(ListFactory.prepend(Symbol.create("if"),
            ListFactory.create(Boolean.FALSE, _sym1, _sym2))) == _val2);
    }

    public void testBegin() throws Exception
    {
        define(_sym2, _val2);

        try
        {
            machine.evaluate(ListFactory.create(Symbol.create("begin"), _sym1,
                _sym2));
            fail("begin failed");
        }
        catch (SchemeException e)
        {}

        _environment.define(_sym1, _val1);

        assertTrue(machine.evaluate(ListFactory.create(Symbol.create("begin"),
            _sym1, _sym2)) == _val2);
    }

    public void testLambdaFailures() throws Exception
    {
        try
        {
            evaluate("(lambda () #(1 2 3))");
            fail("expected CantCompileException");
        }
        catch (CantCompileException e)
        {}

        try
        {
            evaluate("(lambda (#t) #t)");
            fail("expected SymbolExpected");
        }
        catch (SymbolExpected e)
        {}

        try
        {
            evaluate("(lambda (x y x) #t)");
            fail("expected CompileError");
        }
        catch (CompileError e)
        {}
    }

    public void testLambdaNoArgs() throws Exception
    {
        Function func = (Function)machine.evaluate(ListFactory.create(Symbol
                .create("lambda"), ListFactory.create(), _val1));

        assertTrue(machine.evaluate(ListFactory.create(func)) == _val1);

        try
        {
            machine.evaluate(ListFactory.create(func, _unval));
            fail("expected CantEvaluateException");
        }
        catch (CantCompileException e)
        {}

        try
        {
            machine.evaluate(ListFactory.create(func, _val1));
            fail("expected RuntimeArityError");
        }
        catch (RuntimeArityError e)
        {}
    }

    public void testLambdaWithSimpleArgs() throws Exception
    {
        Function func = (Function)evaluate("(lambda (x y) x)");

        assertTrue(machine.evaluate(ListFactory.create(func, _val1, _val2)) == _val1);

        try
        {
            machine.evaluate(ListFactory.create(func, _val1));
            fail("expected RuntimeArityError");
        }
        catch (RuntimeArityError e)
        {}
    }

    public void testLambdaWithOptionalArgs() throws Exception
    {
        Function func = (Function)evaluate("(lambda (x . y) y)");

        try
        {
            machine.evaluate(ListFactory.create(func));
            fail("expected RuntimeArityError");
        }
        catch (RuntimeArityError e)
        {}

        assertTrue(machine.evaluate(ListFactory.create(func, _val1)) == ListFactory
                .create());

        assertTrue(((IList)machine.evaluate(ListFactory.create(func, _val1,
            _val2))).getHead() == _val2);
    }

    public void testLambdaOptionalIsNewList() throws Exception
    {
        Function func = (Function)evaluate("(lambda x x)");

        IPair pair2 = ListFactory.createPair(_val2, ListFactory.create());
        IPair pair1 = ListFactory.createPair(_val1, pair2);

        Object result = machine.evaluate(ListFactory.createPair(func, pair1));

        assertTrue(ValueTraits.equal(result, pair1));
        assertTrue(!ValueTraits.eq(result, pair1));
    }

    public void testDefineNormal() throws Exception
    {
        machine.evaluate(ListFactory.create(Symbol.create("define"), Symbol
                .create("a"), _val1));

        assertTrue(machine.evaluate(Symbol.create("a")) == _val1);
    }

    public void testApplication() throws Exception
    {
        evaluate("(define f (lambda (x) x))");

        assertTrue(machine.evaluate(ListFactory.create(Symbol.create("f"),
            _val1)) == _val1);
    }

    public void testDefineFunction() throws Exception
    {
        evaluate("(define (f x y) x)");
        evaluate("(define (g . x) x)");

        assertTrue("function creation failed", machine.evaluate(Symbol
                .create("f")) instanceof Function);

        assertTrue("function application failed", machine.evaluate(ListFactory
                .create(Symbol.create("f"), _val1, _val2)) == _val1);
    }

    public void testCallCC() throws Exception
    {
        assertTrue(machine.evaluate(ListFactory.create(
            mscheme.values.functions.CallCCFunction.INSTANCE, ListFactory
                    .create(Symbol.create("lambda"), ListFactory.create(Symbol
                            .create("return")), ListFactory.create(Symbol
                            .create("return"), _val1)))) == _val1);
    }
}
