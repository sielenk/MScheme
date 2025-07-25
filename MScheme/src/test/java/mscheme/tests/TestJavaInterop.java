/*
 * Copyright (C) 2025  Marvin H. Sielenkemper
 *
 * This file is part of MScheme.
 *
 * MScheme is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 *
 * MScheme is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with MScheme; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA  02111-1307, USA.
 */

package mscheme.tests;

import mscheme.exceptions.SchemeException;
import mscheme.values.IList;
import mscheme.values.PairOrList;
import mscheme.values.ValueTraits;

public class TestJavaInterop extends TestSchemeBase {

  public TestJavaInterop(String name) {
    super(name);
  }

  public void test_staticField()
      throws InterruptedException, SchemeException, NoSuchFieldException {
    var machine = getMachine();
    var field = getClass().getField("testFieldStatic");

    machine.getEnvironment().define("foo", field);

    testFieldStatic = "something";
    check("(foo)", "something");

    testFieldStatic = "something_else";
    check("(foo)", "something_else");
  }

  public void test_field()
      throws InterruptedException, SchemeException, NoSuchFieldException {
    var machine = getMachine();
    var field = getClass().getField("testField");
    var environment = machine.getEnvironment();

    environment.define("bar", this);
    environment.define("foo", field);

    testField = "something";
    check("(foo bar)", "something");

    testField = "something_else";
    check("(foo bar)", "something_else");
  }

  public void test_staticMethod()
      throws InterruptedException, SchemeException, NoSuchMethodException {
    var machine = getMachine();
    var method = getClass().getMethod("staticMethod");
    var environment = machine.getEnvironment();

    environment.define("foo", method);

    testFieldStatic = "something";
    check("(foo)", "something");

    testFieldStatic = "something_else";
    check("(foo)", "something_else");
  }

  public void test_method()
      throws InterruptedException, SchemeException, NoSuchMethodException {
    var machine = getMachine();
    var method = getClass().getMethod("method");
    var environment = machine.getEnvironment();

    environment.define("bar", this);
    environment.define("foo", method);

    testField = "something";
    check("(foo bar)", "something");

    testField = "something_else";
    check("(foo bar)", "something_else");
  }

  public void test_staticMethodList()
      throws InterruptedException, SchemeException, NoSuchMethodException {
    var machine = getMachine();
    var method = getClass().getMethod("staticMethodList", IList.class);
    var environment = machine.getEnvironment();

    environment.define("foo", method);

    check("(foo)", "()");
    check("(foo 1 2 3)", "(1 2 3)");
  }

  public void test_methodList()
      throws InterruptedException, SchemeException, NoSuchMethodException {
    var machine = getMachine();
    var method = getClass().getMethod("methodList", IList.class);
    var environment = machine.getEnvironment();

    environment.define("bar", this);
    environment.define("foo", method);

    check("(foo bar)", "()");
    check("(foo bar 1 2 3)", "(1 2 3)");
  }

  public void test_staticMethodArgs()
      throws InterruptedException, SchemeException, NoSuchMethodException {
    var machine = getMachine();
    var method = getClass().getMethod("staticMethodArgs", Object.class, Object.class);
    var environment = machine.getEnvironment();

    environment.define("foo", method);

    check("(foo 2 #f)", "(2 . #f)");
    check("(foo '(#t #f) '(1 2 3))", "((#t #f) . (1 2 3))");
  }

  public void test_methodArgs()
      throws InterruptedException, SchemeException, NoSuchMethodException {
    var machine = getMachine();
    var method = getClass().getMethod("methodArgs", Object.class, Object.class);
    var environment = machine.getEnvironment();

    environment.define("bar", this);
    environment.define("foo", method);

    check("(foo bar 1 #t)", "(1 . #t)");
    check("(foo bar '() '())", "(() . ()))");
  }

  public static Object testFieldStatic = null;
  public Object testField = null;

  public static Object staticMethod() {
    return testFieldStatic;
  }

  public Object method() {
    return testField;
  }

  public static Object staticMethodList(IList arguments) {
    return arguments;
  }

  public Object methodList(IList arguments) {
    return arguments;
  }

  public static Object staticMethodArgs(Object fst, Object snd) {
    return PairOrList.create(fst, snd);
  }

  public Object methodArgs(Object fst, Object snd) {
    return PairOrList.create(fst, snd);
  }
}
