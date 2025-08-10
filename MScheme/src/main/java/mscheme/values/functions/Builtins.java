/*
 * Collected implementations of various functions. Copyright (C) 2001 Marvin H.
 * Sielenkemper
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

package mscheme.values.functions;

import java.io.StringReader;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;
import kotlin.reflect.KProperty0;
import kotlin.reflect.KVisibility;
import mscheme.environment.Environment;
import mscheme.exceptions.CharExpected;
import mscheme.exceptions.CloseException;
import mscheme.exceptions.CompileError;
import mscheme.exceptions.ImmutableException;
import mscheme.exceptions.InvalidStringIndexException;
import mscheme.exceptions.ListExpected;
import mscheme.exceptions.NumberExpected;
import mscheme.exceptions.OpenException;
import mscheme.exceptions.PairExpected;
import mscheme.exceptions.PortExpected;
import mscheme.exceptions.RuntimeArityError;
import mscheme.exceptions.SchemeRuntimeError;
import mscheme.exceptions.StringExpected;
import mscheme.exceptions.SymbolExpected;
import mscheme.exceptions.TypeError;
import mscheme.exceptions.VectorException;
import mscheme.exceptions.VectorExpected;
import mscheme.util.Arity;
import mscheme.values.Empty;
import mscheme.values.Function;
import mscheme.values.IList;
import mscheme.values.InputPort;
import mscheme.values.ListFactory;
import mscheme.values.OutputPort;
import mscheme.values.ScmNumber;
import mscheme.values.ScmString;
import mscheme.values.ScmVector;
import mscheme.values.ValueTraits;

final class Order {

  public final static int LT = -2;

  public final static int LE = -1;

  public final static int EQ = 0;

  public final static int GE = 1;

  public final static int GT = 2;

  public static boolean check(IList arguments, int mode)
      throws SchemeRuntimeError, TypeError {
    final Arity arity = Arity.atLeast(2);
    int len = arguments.getLength();

    if (!arity.isValid(len)) {
      throw new RuntimeArityError(arguments, arity);
    }

    ScmNumber curr = ValueTraits.toScmNumber(arguments.getHead());
    IList tail = arguments.getTail();

    boolean rising = true;
    boolean strict = true;
    boolean falling = true;

    do {
      ScmNumber next = ValueTraits.toScmNumber(tail.getHead());
      tail = tail.getTail();

      if (curr.isEqualTo(next)) {
        strict = false;
      } else {
        if (curr.isLessThan(next)) {
          falling = false;
        } else {
          rising = false;
        }

        if (!rising & !falling) {
          return false;
        }
      }

      curr = next;
    }
    while (!tail.isEmpty());

    return switch (mode) {
      case LT -> strict & rising;
      case LE -> rising;
      case EQ -> rising & falling;
      case GE -> falling;
      case GT -> strict & falling;
      default -> false;
    };
  }
}

public class Builtins {

  // 6. Standard procedures

  // 6.1 Equivalence predicates
  public static Object eq_3F(Object fst, Object snd) {
    return ValueTraits.toScmBoolean(ValueTraits.eq(fst, snd));
  }

  public static Object eqv_3F(Object fst, Object snd) {
    return ValueTraits.toScmBoolean(ValueTraits.eqv(fst, snd));
  }

  public static Object equal_3F(Object fst, Object snd) {
    return ValueTraits.toScmBoolean(ValueTraits.equal(fst, snd));
  }

  // 6.2 Numbers

  // 6.2.5 Numerical operations
  public static Object number_3F(Object argument) {
    return ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument));
  }

  public static Object complex_3F(Object argument) {
    return ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument));
  }

  public static Object real_3F(Object argument) {
    return ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument));
  }

  public static Object rational_3F(Object argument) {
    return ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument));
  }

  public static Object integer_3F(Object argument) {
    return ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument));
  }

  public static Object exact_3F(Object argument) {
    return ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument));
  }

  public static Object inexact_3F(Object argument) {
    return ValueTraits.FALSE;
  }

  public static Object _3C(IList arguments)
    // <
      throws SchemeRuntimeError, TypeError {
    return ValueTraits.toScmBoolean(Order.check(arguments, Order.LT));
  }

  public static Object _3C_3D(IList arguments)
    // <=
      throws SchemeRuntimeError, TypeError {
    return ValueTraits.toScmBoolean(Order.check(arguments, Order.LE));
  }

  public static Object _3D(IList arguments)
    // =
      throws SchemeRuntimeError, TypeError {
    return ValueTraits.toScmBoolean(Order.check(arguments, Order.EQ));
  }

  public static Object _3E_3D(IList arguments)
    // >=
      throws SchemeRuntimeError, TypeError {
    return ValueTraits.toScmBoolean(Order.check(arguments, Order.GE));
  }

  public static Object _3E(IList arguments)
    // >
      throws SchemeRuntimeError, TypeError {
    return ValueTraits.toScmBoolean(Order.check(arguments, Order.GT));
  }

  public static Object zero_3F(Object argument)
    // zero?
      throws TypeError {
    return ValueTraits.toScmBoolean(ValueTraits.toScmNumber(argument)
        .getInteger() == 0);
  }

  public static Object _2B(IList arguments)
    // +
      throws SchemeRuntimeError, TypeError {
    ScmNumber sum = ScmNumber.create(0);
    IList tail = arguments;

    while (!tail.isEmpty()) {
      ScmNumber term = ValueTraits.toScmNumber(tail.getHead());
      IList nextTail = tail.getTail();

      sum = sum.plus(term);
      tail = nextTail;
    }
    return sum;
  }

  public static Object _2D(IList arguments)
    // -
      throws SchemeRuntimeError, TypeError {
    ScmNumber result = ValueTraits.toScmNumber(arguments.getHead());
    IList rest = arguments.getTail();

    if (!rest.isEmpty()) {
      do {
        ScmNumber head = ValueTraits.toScmNumber(rest.getHead());
        IList tail = rest.getTail();

        result = result.minus(head);
        rest = tail;
      }
      while (!rest.isEmpty());

      return result;
    } else {
      return result.negated();
    }
  }

  public static Object _2A(IList arguments)
    // *
      throws SchemeRuntimeError, TypeError {
    ScmNumber product = ScmNumber.create(1);
    IList tail = arguments;

    while (!tail.isEmpty()) {
      ScmNumber factor = ValueTraits.toScmNumber(tail.getHead());
      IList nextTail = tail.getTail();

      product = product.times(factor);
      tail = nextTail;
    }

    return product;
  }

  public static Object _2F(IList arguments)
    // /
      throws SchemeRuntimeError, TypeError {
    ScmNumber result = ValueTraits.toScmNumber(arguments.getHead());
    IList rest = arguments.getTail();

    if (!rest.isEmpty()) {
      do {
        ScmNumber head = ValueTraits.toScmNumber(rest.getHead());
        IList tail = rest.getTail();

        result = result.divide(head);
        rest = tail;
      }
      while (!rest.isEmpty());

      return result;
    } else {
      return result.reciprocal();
    }
  }

  // 6.3 Other data types

  // 6.3.1 Booleans

  public static Object not(Object argument) {
    return ValueTraits.toScmBoolean(!ValueTraits.isTrue(argument));
  }

  public static Object boolean_3F(Object argument) // boolean?
  {
    return ValueTraits.toScmBoolean(ValueTraits.isScmBoolean(argument));
  }

  // 6.3.2 Pairs and lists

  public static Object pair_3F(Object argument) // pair?
  {
    return ValueTraits.toScmBoolean(ValueTraits.isPair(argument));
  }

  public static Object cons(Object fst, Object snd) {
    return ListFactory.createPair(fst, snd);
  }

  public static Object car(Object argument)
      throws PairExpected {
    return ValueTraits.toConstPair(argument).getFirst();
  }

  public static Object cdr(Object argument)
      throws PairExpected {
    return ValueTraits.toConstPair(argument).getSecond();
  }

  public static Object set_2Dcar_21(Object fst, Object snd)
    // set-car!
      throws PairExpected, ImmutableException {
    ValueTraits.toMutablePair(fst).setFirst(snd);
    return snd;
  }

  public static Object set_2Dcdr_21(Object fst, Object snd)
    // set-car!
      throws PairExpected, ImmutableException {
    ValueTraits.toMutablePair(fst).setSecond(snd);
    return snd;
  }

  public static Object null_3F(Object argument) // null?
  {
    return ValueTraits.toScmBoolean(argument instanceof Empty);
  }

  public static Object list_3F(Object argument) // list?
  {
    return ValueTraits.toScmBoolean(ValueTraits.isList(argument));
  }

  public static Object list(IList argument)
      throws ListExpected {
    // Without first-class continuations, it would be save to
    // omit the call to getCopy(). But multiple returns have to
    // return different lists ...
    return argument.getCopy();
  }

  public static Object length(Object argument)
      throws ListExpected {
    return ScmNumber.create(ValueTraits.toList(argument).getLength());
  }

  public final static Function append = AppendFunction.INSTANCE;

  public static Object reverse(Object argument)
      throws ListExpected {
    return ValueTraits.toList(argument).getReversed();
  }

  public final static Function memq = MemqFunction.INSTANCE;

  public final static Function memv = MemvFunction.INSTANCE;

  public final static Function member = MemberFunction.INSTANCE;

  public final static Function assq = AssqFunction.INSTANCE;

  public final static Function assv = AssvFunction.INSTANCE;

  public final static Function assoc = AssocFunction.INSTANCE;

  // 6.3.3 Symbols
  public static Object symbol_3F(Object argument) // symbol?
  {
    return ValueTraits.toScmBoolean(ValueTraits.isSymbol(argument));
  }

  public static Object symbol_2D_3Estring(Object argument)
    // symbol->string
      throws SymbolExpected {
    return ScmString.createConst(ValueTraits.toSymbol(argument));
  }

  public static Object string_2D_3Esymbol(Object argument)
    // string->symbol
      throws StringExpected {
    return ValueTraits.toScmString(argument).getJavaString();
  }

  // 6.3.4 Characters

  public static Object char_3F(Object argument) // char?
  {
    return ValueTraits.toScmBoolean(ValueTraits.isScmChar(argument));
  }

  public static Object char_3C_3F(Object fst, Object snd)
    // char<?
      throws CharExpected {
    return ValueTraits
        .toScmBoolean(ValueTraits.toScmChar(fst) < ValueTraits
            .toScmChar(snd));
  }

  public static Object char_3C_3D_3F(Object fst, Object snd)
    // char<=?
      throws CharExpected {
    return ValueTraits
        .toScmBoolean(ValueTraits.toScmChar(fst) <= ValueTraits
            .toScmChar(snd));
  }

  public static Object char_3D_3F(Object fst, Object snd)
    // char=?
      throws CharExpected {
    return ValueTraits.toScmBoolean(
        ValueTraits.toScmChar(fst) == ValueTraits.toScmChar(snd)
    );
  }

  public static Object char_3E_3D_3F(Object fst, Object snd)
    // char>=?
      throws CharExpected {
    return ValueTraits
        .toScmBoolean(ValueTraits.toScmChar(fst) >= ValueTraits
            .toScmChar(snd));
  }

  public static Object char_3E_3F(Object fst, Object snd)
    // char>?
      throws CharExpected {
    return ValueTraits
        .toScmBoolean(ValueTraits.toScmChar(fst) > ValueTraits
            .toScmChar(snd));
  }

  public static Object char_2D_3Einteger(Object argument)
      throws CharExpected {
    return ScmNumber.create(ValueTraits.toScmChar(argument));
  }

  public static Object integer_2D_3Echar(Object argument)
      throws NumberExpected {
    return ValueTraits.toScmChar((char) ValueTraits.toScmNumber(argument)
        .getInteger());
  }

  public static Object char_2Dupcase(Object argument)
      throws CharExpected {
    return ValueTraits.toScmChar(Character.toUpperCase(ValueTraits
        .toScmChar(argument)));
  }

  public static Object char_2Ddowncase(Object argument)
      throws CharExpected {
    return ValueTraits.toScmChar(Character.toLowerCase(ValueTraits
        .toScmChar(argument)));
  }

  // 6.3.5 Strings

  public static Object string_3F(Object argument) // string?
  {
    return ValueTraits.toScmBoolean(ValueTraits.isScmString(argument));
  }

  public static Object make_2Dstring(Object k, Object c)
      throws TypeError {
    return ScmString.create(ValueTraits.toScmNumber(k).getInteger(),
        ValueTraits.toScmChar(c));
  }

  public static Object string_2Dlength(Object str)
      throws TypeError {
    return ScmNumber.create(ValueTraits.toScmString(str).getLength());
  }

  public static Object string_2Dref(Object str, Object k)
      throws TypeError, InvalidStringIndexException {
    return ValueTraits.toScmChar(ValueTraits.toScmString(str).get(
        ValueTraits.toScmNumber(k).getInteger()));
  }

  public static Object string_2Dset_21(Object str, Object k, Object c)
      throws TypeError, InvalidStringIndexException, ImmutableException {
    ValueTraits.toScmString(str).set(
        ValueTraits.toScmNumber(k).getInteger(),
        ValueTraits.toScmChar(c));

    return c;
  }

  public static Object string_2Dappend(IList arguments)
      throws TypeError, InvalidStringIndexException, ImmutableException {
    StringBuilder accu = new StringBuilder();

    for (IList rest = arguments; !rest.isEmpty(); rest = rest.getTail()) {
      accu
          .append(ValueTraits.toScmString(rest.getHead())
              .getJavaString());
    }

    return ScmString.create(accu.toString());
  }

  public static Object string_2Dcopy(Object string)
      throws TypeError, InvalidStringIndexException, ImmutableException {
    return ScmString
        .create(ValueTraits.toScmString(string).getJavaString());
  }

  public static Object string_2D_3Elist(Object scmString)
      throws TypeError, InvalidStringIndexException, ImmutableException {
    String javaString = ValueTraits.toScmString(scmString).getJavaString();
    IList result = ListFactory.create();

    for (int i = javaString.length() - 1; i >= 0; --i) {
      result = ListFactory.prepend(ValueTraits.toScmChar(javaString
          .charAt(i)), result);
    }

    return result;
  }

  public static Object list_2D_3Estring(Object list)
      throws TypeError, InvalidStringIndexException, ImmutableException {
    StringBuilder accu = new StringBuilder();

    for (IList rest = ValueTraits.toList(list); !rest.isEmpty(); rest = rest
        .getTail()) {
      accu.append(ValueTraits.toScmChar(rest.getHead()));
    }

    return ScmString.create(accu.toString());
  }

  // 6.3.6 Vectors

  public static Object vector_3F(Object argument) // vector?
  {
    return ValueTraits.toScmBoolean(ValueTraits.isScmVector(argument));
  }

  public static Object make_2Dvector(Object k, Object obj)
      throws TypeError {
    return ScmVector.create(ValueTraits.toScmNumber(k).getInteger(), obj);
  }

  public static Object vector_2Dlength(Object str)
      throws TypeError {
    return ScmNumber.create(ValueTraits.toScmVector(str).getLength());
  }

  public static Object vector_2Dref(Object vector, Object k)
      throws TypeError, VectorException {
    return ValueTraits.toScmVector(vector).get(
        ValueTraits.toScmNumber(k).getInteger());
  }

  public static Object vector_2Dset_21(Object vector, Object k,
      Object obj)
      throws TypeError, VectorException, ImmutableException {
    ValueTraits.toScmVector(vector).set(
        ValueTraits.toScmNumber(k).getInteger(), obj);

    return obj;
  }

  public static Object vector(IList arguments)
    // vector
      throws ListExpected {
    return ScmVector.create(arguments);
  }

  public static Object vector_2D_3Elist(Object argument)
    // vector->list
      throws VectorExpected {
    return ValueTraits.toScmVector(argument).getList();
  }

  public static Object list_2D_3Evector(Object argument)
    // list->vector
      throws ListExpected {
    return ScmVector.create(ValueTraits.toList(argument));
  }

  // 6.4 Control features

  public static Object procedure_3F(Object argument) // procedure?
  {
    return ValueTraits.toScmBoolean(ValueTraits.isFunction(argument));
  }

  public final static Function apply = ApplyFunction.INSTANCE;

  public final static Function call_2Dwith_2Dcurrent_2Dcontinuation = CallCCFunction.INSTANCE;

  //  public final static Function dynamic_2Dwind =
  // DynamicWindFunction.INSTANCE;

  // 6.5 Eval

  public final static Function eval = EvalFunction.INSTANCE;

  public static Object scheme_2Dreport_2Denvironment(Object fst)
      throws SchemeRuntimeError, TypeError {
    if (ValueTraits.toScmNumber(fst).getInteger() != 5) {
      throw new SchemeRuntimeError(fst);
    }

    return Environment.getSchemeReportEnvironment();
  }

  public static Object null_2Denvironment(Object fst)
      throws SchemeRuntimeError, TypeError {
    if (ValueTraits.toScmNumber(fst).getInteger() != 5) {
      throw new SchemeRuntimeError(fst);
    }

    return Environment.getNullEnvironment();
  }

  // 6.6 Input and output

  // 6.6.1 Ports

  public static Object port_3F(Object argument) // port?
  {
    return ValueTraits.toScmBoolean(ValueTraits.isPort(argument));
  }

  public static Object input_2Dport_3F(Object argument)
    // input-port?
      throws PortExpected {
    return ValueTraits.toScmBoolean(argument instanceof InputPort);
  }

  public static Object output_2Dport_3F(Object argument)
    // output-port?
      throws PortExpected {
    return ValueTraits.toScmBoolean(argument instanceof OutputPort);
  }

  public static Object open_2Dinput_2Dfile(Object argument)
      throws StringExpected, OpenException {
    return InputPort.create(ValueTraits.toScmString(argument));
  }

  public static Object open_2Doutput_2Dfile(Object argument)
      throws StringExpected, OpenException {
    return OutputPort.create(ValueTraits.toScmString(argument));
  }

  public static Object close_2Dinput_2Dport(Object argument)
      throws PortExpected, CloseException {
    ValueTraits.toInputPort(argument).close();
    return argument;
  }

  public static Object close_2Doutput_2Dport(Object argument)
      throws PortExpected, CloseException {
    ValueTraits.toOutputPort(argument).close();
    return argument;
  }

  // 6.6.2 Input

  public static Object read(Object fst)
      throws SchemeRuntimeError, TypeError {
    try {
      return ValueTraits.toInputPort(fst).read();
    } catch (InterruptedException e) {
      throw new SchemeRuntimeError(fst, e.toString());
    }
  }

  public static Object read_2Dchar(Object fst)
      throws SchemeRuntimeError, TypeError {
    return ValueTraits.toInputPort(fst).readScmChar();
  }

  public static Object peek_2Dchar(Object fst)
      throws SchemeRuntimeError, TypeError {
    return ValueTraits.toInputPort(fst).peekScmChar();
  }

  public static Object eof_2Dobject_3F(Object fst) {
    return ValueTraits.toScmBoolean(ValueTraits
        .eq(fst, InputPort.EOF_VALUE));
  }

  public static Object char_2Dready_3F(Object fst)
      throws TypeError {
    return ValueTraits.toScmBoolean(ValueTraits.toInputPort(fst).isReady());
  }

  // 6.6.3 Output

  public static Object write(Object fst, Object snd)
      throws SchemeRuntimeError, TypeError {
    ValueTraits.toOutputPort(snd).write(fst);
    return snd;
  }

  public static Object display(Object fst, Object snd)
      throws SchemeRuntimeError, TypeError {
    ValueTraits.toOutputPort(snd).display(fst);
    return snd;
  }

  public static Object write_2Dchar(Object fst, Object snd)
      throws SchemeRuntimeError, TypeError {
    ValueTraits.toOutputPort(snd).writeScmChar(ValueTraits.toScmChar(fst));
    return snd;
  }

  // additional functions

  public static Object __unique_2Did() {
    return ValueTraits.createUniqueSymbol();
  }

  public final static Function __spawn = SpawnFunction.INSTANCE;

  public final static Function __y_2Dcombinator = YCombinator.INSTANCE;

  public static Object __open_2Dinput_2Dstring(Object argument)
      throws TypeError {
    return InputPort.create(new StringReader(ValueTraits.toScmString(
        argument).getJavaString()));
  }

  //  not very usefull yet ... needs GET-OUTPUT-STRING
  //  public final static Object __open_2Doutput_2Dstring()
  //      throws TypeError
  //  {
  //      return OutputPort.create(
  //          new StringWriter()
  //      );
  //  }

  public static void getBuiltins(Environment target) throws CompileError {
    var jClass = Builtins.class;
    var kClass = kotlin.jvm.JvmClassMappingKt.getKotlinClass(jClass);
    var members = kClass.getMembers();

    Map<String, Function> kFields = Map.ofEntries(
        members.stream()
            .filter(it -> it instanceof KProperty0<?>
                && it.getVisibility() == KVisibility.PUBLIC)
            .map(it -> Map.entry(it.getName(), ((KProperty0<?>) it).get()))
            .filter(it -> it.getValue() instanceof Function)
            .map(it -> Map.entry(parseName(it.getKey()),
                (Function) it.getValue()))
            .toArray(Entry[]::new)
    );

    kFields.forEach((schemeName, function) -> {
          try {
            target.define(schemeName, function);
          } catch (Exception e) {
            System.err.println(
                "Error while defining " + schemeName + ": " + e.getMessage());
          }
        }
    );

    var allMethods = jClass.getMethods();
    var methods = Arrays.stream(allMethods).filter(me -> {
      int mo = me.getModifiers();

      if (!Modifier.isStatic(mo) || !Modifier.isPublic(mo)) {
        return false;
      }

      Class<?>[] parameterTypes = me.getParameterTypes();

      if (!paramsValid(parameterTypes)) {
        return false;
      }

      return Object.class.isAssignableFrom(me.getReturnType());
    }).toList();

    for (Method me : methods) {
      final String schemeName = parseName(me.getName());

      target.define(schemeName, me);
    }
  }

  private static String parseName(String name) {
    StringBuilder buf = new StringBuilder();

    for (
        int index = name.startsWith("__") ? 2 : 0;
        index < name.length();
        ++index
    ) {
      char c = name.charAt(index);

      if (c == '_') {
        buf.append(
            (char) Integer.parseInt(
                name.substring(index + 1, index + 3),
                16
            )
        );
        index += 2;
      } else {
        buf.append(c);
      }
    }

    return buf.toString();
  }

  private static boolean paramsValid(Class<?>[] params) {
    if (params.length == 1) {
      return (params[0] == Object.class) || (params[0] == IList.class);
    } else {
      for (Class<?> param : params) {
        if (param != Object.class) {
          return false;
        }
      }
    }

    return true;
  }
}
