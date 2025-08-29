/*
 * Copyright (C) 2001 Marvin H. Sielenkemper
 *
 * This file is part of MScheme.
 *
 * MScheme is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * MScheme is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * MScheme; see the file COPYING. If not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

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
import mscheme.values.IList;
import mscheme.values.IPair;
import mscheme.values.InputPort;
import mscheme.values.ListFactory;
import mscheme.values.ValueTraits;

public class TestMachine
    extends TestCase {

  private Machine machine;

  private String _sym1;

  private String _sym2;

  private Object _val1;

  private Object _val2;

  private Object _unval;

  private Environment _environment;

  public TestMachine(String name) {
    super(name);
  }

  protected void setUp()
      throws Exception {
    _sym1 = "test1";
    _sym2 = "test2";

    _val1 = Boolean.TRUE;
    _val2 = Boolean.FALSE;
    _unval = ListFactory.INSTANCE.create();

    _environment = Environment.Companion.getNullEnvironment();

    machine = new Machine(_environment);
  }

  protected void tearDown() {
    machine = null;
    _environment = null;
  }

  private Object evaluate(String expression)
      throws SchemeException, InterruptedException {
    return machine.evaluate(
        InputPort.Companion.create(new StringReader(expression)).read()
    );
  }

  public void testTestValues()
      throws Exception {
    Compiler compiler = new Compiler(_environment.getStatic());

    try {
      compiler.compile(_unval);
      fail("expected CantCompileException");
    } catch (CantCompileException e) {
    }

    assertNotNull(compiler.compile(_val1));
    assertNotNull(compiler.compile(_val2));
  }

  public void testEnvironment() {
    assertSame(_environment, machine.getEnvironment());
  }

  public void testValue()
      throws Exception {
    assertSame(_val1, machine.evaluate(_val1));
    assertSame(_val2, machine.evaluate(_val2));
  }

  public void testUnevaluatable()
      throws Exception {
    try {
      machine.evaluate(_unval);
      fail("evaluated Unevaluatable");
    } catch (CantCompileException e) {
    }
  }

  private void define(String s, Object v)
      throws Exception {
    _environment.define(s, v);
  }

  public void testSymbol()
      throws Exception {
    try {
      machine.evaluate(_sym1);
      fail("expected SymbolNotFoundException");
    } catch (SchemeException e) {
    }

    define(_sym1, _val1);
    define(_sym2, _unval);

    assertSame("evaluation to Value failed - ", _val1, machine
        .evaluate(_sym1));
    assertSame("evaluation to Unevaluatable failed - ", _unval, machine
        .evaluate(_sym2));
  }

  public void testPair1()
      throws Exception {
    try {
      machine.evaluate(ListFactory.INSTANCE.createPair(_val1, _val2));
      fail("expected ListExpected");
    } catch (ListExpected e) {
    }
  }

  public void testPair2()
      throws Exception {
    try {
      machine.evaluate(ListFactory.INSTANCE.create(_val1));
      fail("expected FunctionExpected");
    } catch (FunctionExpected e) {
    }
  }

  public void testQuote()
      throws Exception {
    assertSame(
        _unval,
        machine.evaluate(
            ListFactory.INSTANCE.create(
                "quote",
                _unval)));
  }

  public void testIf()
      throws Exception {
    define(_sym1, _val1);
    define(_sym2, _val2);

    assertSame(_val1, machine.evaluate(ListFactory.INSTANCE.prepend("if",
        ListFactory.INSTANCE.create(Boolean.TRUE, _sym1, _sym2))));

    assertSame(_val1, machine.evaluate(ListFactory.INSTANCE.prepend("if",
        ListFactory.INSTANCE.create(Boolean.TRUE, _sym1))));

    assertSame(_val2, machine
        .evaluate(ListFactory.INSTANCE.prepend("if", ListFactory
            .INSTANCE.create(Boolean.FALSE, _sym1, _sym2))));
  }

  public void testBegin()
      throws Exception {
    define(_sym2, _val2);

    try {
      machine.evaluate(ListFactory.INSTANCE.create("begin", _sym1,
          _sym2));
      fail("begin failed");
    } catch (SchemeException e) {
    }

    _environment.define(_sym1, _val1);

    assertSame(_val2, machine.evaluate(
        ListFactory.INSTANCE.create("begin", _sym1, _sym2)));
  }

  public void testLambdaFailures()
      throws Exception {
    try {
      evaluate("(lambda () #(1 2 3))");
      fail("expected CantCompileException");
    } catch (CantCompileException e) {
    }

    try {
      evaluate("(lambda (#t) #t)");
      fail("expected SymbolExpected");
    } catch (SymbolExpected e) {
    }

    try {
      evaluate("(lambda (x y x) #t)");
      fail("expected CompileError");
    } catch (CompileError e) {
    }
  }

  public void testLambdaNoArgs()
      throws Exception {
    Function func = (Function) machine.evaluate(
        ListFactory.INSTANCE.create("lambda",
            ListFactory.INSTANCE.create(), _val1));

    assertSame(_val1, machine.evaluate(ListFactory.INSTANCE.create(func)));

    try {
      machine.evaluate(ListFactory.INSTANCE.create(func, _unval));
      fail("expected CantEvaluateException");
    } catch (CantCompileException e) {
    }

    try {
      machine.evaluate(ListFactory.INSTANCE.create(func, _val1));
      fail("expected RuntimeArityError");
    } catch (RuntimeArityError e) {
    }
  }

  public void testLambdaWithSimpleArgs()
      throws Exception {
    Function func = (Function) evaluate("(lambda (x y) x)");

    assertSame(_val1, machine.evaluate(ListFactory.INSTANCE.create(func, _val1,
        _val2)));

    try {
      machine.evaluate(ListFactory.INSTANCE.create(func, _val1));
      fail("expected RuntimeArityError");
    } catch (RuntimeArityError e) {
    }
  }

  public void testLambdaWithOptionalArgs()
      throws Exception {
    Function func = (Function) evaluate("(lambda (x . y) y)");

    try {
      machine.evaluate(ListFactory.INSTANCE.create(func));
      fail("expected RuntimeArityError");
    } catch (RuntimeArityError e) {
    }

    assertSame(ListFactory.INSTANCE.create(),
        machine.evaluate(ListFactory.INSTANCE.create(
            func, _val1)));

    assertSame(_val2, ((IList) machine.evaluate(ListFactory.INSTANCE.create(func,
        _val1, _val2))).getHead());
  }

  public void testLambdaOptionalIsNewList()
      throws Exception {
    Function func = (Function) evaluate("(lambda x x)");

    IPair pair2 = ListFactory.INSTANCE.createPair(_val2, ListFactory.INSTANCE.create());
    IPair pair1 = ListFactory.INSTANCE.createPair(_val1, pair2);

    Object result = machine.evaluate(ListFactory.INSTANCE.createPair(func, pair1));

    assertTrue(ValueTraits.INSTANCE.equal(result, pair1));
    assertFalse(ValueTraits.INSTANCE.eq(result, pair1));
  }

  public void testDefineNormal()
      throws Exception {
    machine.evaluate(ListFactory.INSTANCE.create("define", "a", _val1));

    assertSame(_val1, machine.evaluate("a"));
  }

  public void testApplication()
      throws Exception {
    evaluate("(define f (lambda (x) x))");

    assertSame(_val1, machine.evaluate(ListFactory.INSTANCE.create("f", _val1)));
  }

  public void testDefineFunction()
      throws Exception {
    evaluate("(define (f x y) x)");
    evaluate("(define (g . x) x)");

    assertTrue("function creation failed - ",
        machine.evaluate("f") instanceof Function);

    assertSame("function application failed - ", _val1, machine
        .evaluate(ListFactory.INSTANCE.create("f", _val1, _val2)));
  }

  public void testCallCC()
      throws Exception {
    assertSame(_val1, machine.evaluate(ListFactory.INSTANCE.create(
        mscheme.values.functions.CallCCFunction.INSTANCE, ListFactory
            .INSTANCE.create("lambda",
                ListFactory.INSTANCE.create("return"),
                ListFactory.INSTANCE.create("return", _val1)))));
  }
}
