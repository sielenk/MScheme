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
package mscheme.values.functions

import mscheme.environment.Environment.Companion.getNullEnvironment
import mscheme.environment.Environment.Companion.getSchemeReportEnvironment
import mscheme.exceptions.*
import mscheme.machine.Registers
import mscheme.util.Arity
import mscheme.values.*
import mscheme.values.Function
import mscheme.values.ListFactory.createPair
import mscheme.values.ListFactory.prepend
import mscheme.values.ScmString.Companion.createConst
import mscheme.values.ValueTraits.createUniqueSymbol
import mscheme.values.ValueTraits.eq
import mscheme.values.ValueTraits.equal
import mscheme.values.ValueTraits.eqv
import mscheme.values.ValueTraits.isFunction
import mscheme.values.ValueTraits.isList
import mscheme.values.ValueTraits.isPair
import mscheme.values.ValueTraits.isPort
import mscheme.values.ValueTraits.isScmBoolean
import mscheme.values.ValueTraits.isScmChar
import mscheme.values.ValueTraits.isScmNumber
import mscheme.values.ValueTraits.isScmString
import mscheme.values.ValueTraits.isScmVector
import mscheme.values.ValueTraits.isSymbol
import mscheme.values.ValueTraits.isTrue
import mscheme.values.ValueTraits.toConstPair
import mscheme.values.ValueTraits.toInputPort
import mscheme.values.ValueTraits.toList
import mscheme.values.ValueTraits.toMutablePair
import mscheme.values.ValueTraits.toOutputPort
import mscheme.values.ValueTraits.toScmChar
import mscheme.values.ValueTraits.toScmNumber
import mscheme.values.ValueTraits.toScmString
import mscheme.values.ValueTraits.toScmVector
import mscheme.values.ValueTraits.toSymbol
import mscheme.values.functions.Order.Companion.check
import java.io.StringReader


object Builtins {
    // 6. Standard procedures
    // 6.1 Equivalence predicates
    fun eq_3F(fst: Any?, snd: Any?): Any =
        ValueTraits.toScmBoolean(eq(fst, snd))

    fun eqv_3F(fst: Any?, snd: Any?): Any =
        ValueTraits.toScmBoolean(eqv(fst, snd))

    fun equal_3F(fst: Any?, snd: Any?): Any =
        ValueTraits.toScmBoolean(equal(fst, snd))

    // 6.2 Numbers
    // 6.2.5 Numerical operations
    fun number_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(isScmNumber(argument))

    fun complex_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(isScmNumber(argument))

    fun real_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(isScmNumber(argument))

    fun rational_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(isScmNumber(argument))

    fun integer_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(isScmNumber(argument))

    fun exact_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(isScmNumber(argument))

    fun inexact_3F(argument: Any?): Any =
        ValueTraits.FALSE

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun _3C(arguments: IList): Any =
        ValueTraits.toScmBoolean(check(arguments, Order.LT))

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun _3C_3D(arguments: IList): Any =
        ValueTraits.toScmBoolean(check(arguments, Order.LE))

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun _3D(arguments: IList): Any =
        ValueTraits.toScmBoolean(check(arguments, Order.EQ))

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun _3E_3D(arguments: IList): Any =
        ValueTraits.toScmBoolean(check(arguments, Order.GE))

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun _3E(arguments: IList): Any =
        ValueTraits.toScmBoolean(check(arguments, Order.GT))

    @Throws(TypeError::class)
    fun zero_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(
            toScmNumber(argument).integer == 0
        )

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun _2B(arguments: IList): Any {
        var sum = ScmNumber.create(0)
        var tail = arguments

        while (!tail.isEmpty) {
            val term = toScmNumber(tail.head)
            val nextTail = tail.tail

            sum = sum.plus(term)
            tail = nextTail
        }

        return sum
    }

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun _2D(arguments: IList): Any {
        var result = toScmNumber(arguments.head)
        var rest = arguments.tail

        if (!rest.isEmpty) {
            do {
                val head = toScmNumber(rest.head)
                val tail = rest.tail

                result = result.minus(head)
                rest = tail
            } while (!rest.isEmpty)

            return result
        } else {
            return result.negated()
        }
    }

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun _2A(arguments: IList): Any {
        var product = ScmNumber.create(1)
        var tail = arguments

        while (!tail.isEmpty) {
            val factor = toScmNumber(tail.head)
            val nextTail = tail.tail

            product = product.times(factor)
            tail = nextTail
        }

        return product
    }

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun _2F(arguments: IList): Any {
        var result = toScmNumber(arguments.head)
        var rest = arguments.tail

        if (!rest.isEmpty) {
            do {
                val head = toScmNumber(rest.head)
                val tail = rest.tail

                result = result.divide(head)
                rest = tail
            } while (!rest.isEmpty)

            return result
        } else {
            return result.reciprocal()
        }
    }

    // 6.3 Other data types
    // 6.3.1 Booleans
    fun not(argument: Any?): Any =
        ValueTraits.toScmBoolean(!isTrue(argument))

    fun boolean_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(isScmBoolean(argument))

    // 6.3.2 Pairs and lists
    fun pair_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(isPair(argument))

    fun cons(fst: Any?, snd: Any?): Any =
        createPair(fst, snd)

    @Throws(PairExpected::class)
    fun car(argument: Any?): Any? =
        toConstPair(argument).first

    @Throws(PairExpected::class)
    fun cdr(argument: Any?): Any? =
        toConstPair(argument).second

    @Throws(PairExpected::class, ImmutableException::class)
    fun set_2Dcar_21(fst: Any?, snd: Any?): Any? {
        toMutablePair(fst).first = snd
        return snd
    }

    @Throws(PairExpected::class, ImmutableException::class)
    fun set_2Dcdr_21(fst: Any?, snd: Any?): Any? {
        toMutablePair(fst).second = snd
        return snd
    }

    fun null_3F(argument: Any?) = // null?
        ValueTraits.toScmBoolean(argument is Empty)

    fun list_3F(argument: Any?): Any = // list?
        ValueTraits.toScmBoolean(isList(argument))

    @Throws(ListExpected::class)
    fun list(argument: IList): Any {
        // Without first-class continuations, it would be safe to
        // omit the call to getCopy(). But multiple returns have to
        // return different lists ...
        return argument.getCopy()
    }

    @Throws(ListExpected::class)
    fun length(argument: Any?): Any =
        ScmNumber.create(toList(argument).length)

    val append: Function =
        AppendFunction

    @Throws(ListExpected::class)
    fun reverse(argument: Any?): Any =
        toList(argument).getReversed()

    val memq: Function =
        MemqFunction

    val memv: Function =
        MemvFunction

    val member: Function =
        MemberFunction

    val assq: Function =
        AssqFunction

    val assv: Function =
        AssvFunction

    val assoc: Function =
        AssocFunction

    // 6.3.3 Symbols
    fun symbol_3F(argument: Any?): Any = // symbol?
        ValueTraits.toScmBoolean(isSymbol(argument))

    @Throws(SymbolExpected::class)
    fun symbol_2D_3Estring(argument: Any?): Any =
        createConst(toSymbol(argument))

    @Throws(StringExpected::class)
    fun string_2D_3Esymbol(argument: Any?): Any =
        toScmString(argument).javaString

    // 6.3.4 Characters
    fun char_3F(argument: Any?) = // char?
        ValueTraits.toScmBoolean(isScmChar(argument))

    @Throws(CharExpected::class)
    fun char_3C_3F(fst: Any?, snd: Any?): Any =
        ValueTraits.toScmBoolean(toScmChar(fst) < toScmChar(snd))

    @Throws(CharExpected::class)
    fun char_3C_3D_3F(fst: Any?, snd: Any?): Any {
        return ValueTraits.toScmBoolean(
            toScmChar(fst) <= toScmChar(snd)
        )
    }

    @Throws(CharExpected::class)
    fun char_3D_3F(fst: Any?, snd: Any?): Any =
        ValueTraits.toScmBoolean(
            toScmChar(fst) == toScmChar(snd)
        )

    @Throws(CharExpected::class)
    fun char_3E_3D_3F(fst: Any?, snd: Any?): Any =
        ValueTraits.toScmBoolean(
            toScmChar(fst) >= toScmChar(snd)
        )

    @Throws(CharExpected::class)
    fun char_3E_3F(fst: Any?, snd: Any?): Any =
        ValueTraits.toScmBoolean(
            toScmChar(fst) > toScmChar(snd)
        )

    @Throws(CharExpected::class)
    fun char_2D_3Einteger(argument: Any?): Any =
        ScmNumber.create(toScmChar(argument).code)

    @Throws(NumberExpected::class)
    fun integer_2D_3Echar(argument: Any?): Any =
        ValueTraits.toScmChar(
            toScmNumber(argument).integer.toChar()
        )

    @Throws(CharExpected::class)
    fun char_2Dupcase(argument: Any?): Any =
        ValueTraits.toScmChar(toScmChar(argument).uppercaseChar())

    @Throws(CharExpected::class)
    fun char_2Ddowncase(argument: Any?): Any =
        ValueTraits.toScmChar(toScmChar(argument).lowercaseChar())

    // 6.3.5 Strings
    fun string_3F(argument: Any?): Any = // string?
        ValueTraits.toScmBoolean(isScmString(argument))

    @Throws(TypeError::class)
    fun make_2Dstring(k: Any?, c: Any?): Any =
        ScmString.create(
            toScmNumber(k).integer,
            toScmChar(c)
        )

    @Throws(TypeError::class)
    fun string_2Dlength(str: Any?): Any =
        ScmNumber.create(toScmString(str).length)

    @Throws(TypeError::class, InvalidStringIndexException::class)
    fun string_2Dref(str: Any?, k: Any?): Any =
        ValueTraits.toScmChar(
            toScmString(str).get(
                toScmNumber(k).integer
            )
        )

    @Throws(
        TypeError::class,
        InvalidStringIndexException::class,
        ImmutableException::class
    )
    fun string_2Dset_21(str: Any?, k: Any?, c: Any?): Any? {
        toScmString(str).set(
            toScmNumber(k).integer,
            toScmChar(c)
        )

        return c
    }

    @Throws(
        TypeError::class,
        InvalidStringIndexException::class,
        ImmutableException::class
    )
    fun string_2Dappend(arguments: IList): Any {
        val accu = StringBuilder()

        var rest = arguments
        while (!rest.isEmpty) {
            accu.append(
                toScmString(rest.head).javaString
            )
            rest = rest.tail
        }

        return ScmString.create(accu.toString())
    }

    @Throws(
        TypeError::class,
        InvalidStringIndexException::class,
        ImmutableException::class
    )
    fun string_2Dcopy(string: Any?): Any =
        ScmString.create(toScmString(string).javaString)

    @Throws(
        TypeError::class,
        InvalidStringIndexException::class,
        ImmutableException::class
    )
    fun string_2D_3Elist(scmString: Any?): Any {
        val javaString = toScmString(scmString).javaString
        var result = ListFactory.create()

        for (i in javaString.length - 1 downTo 0) {
            result = prepend(
                ValueTraits.toScmChar(javaString[i]),
                result
            )
        }

        return result
    }

    @Throws(
        TypeError::class,
        InvalidStringIndexException::class,
        ImmutableException::class
    )
    fun list_2D_3Estring(list: Any?): Any {
        val accu = StringBuilder()

        var rest = toList(list)
        while (!rest.isEmpty) {
            accu.append(toScmChar(rest.head))
            rest = rest.tail
        }

        return ScmString.create(accu.toString())
    }

    // 6.3.6 Vectors
    fun vector_3F(argument: Any?) = // vector?
        ValueTraits.toScmBoolean(isScmVector(argument))

    @Throws(TypeError::class)
    fun make_2Dvector(k: Any?, obj: Any?): Any =
        ScmVector.create(toScmNumber(k).integer, obj)

    @Throws(TypeError::class)
    fun vector_2Dlength(str: Any?): Any =
        ScmNumber.create(toScmVector(str).length)

    @Throws(TypeError::class, VectorException::class)
    fun vector_2Dref(vector: Any?, k: Any?): Any? =
        toScmVector(vector).get(
            toScmNumber(k).integer
        )

    @Throws(TypeError::class, VectorException::class, ImmutableException::class)
    fun vector_2Dset_21(vector: Any?, k: Any?, obj: Any?): Any? {
        toScmVector(vector).set(
            toScmNumber(k).integer, obj
        )

        return obj
    }

    @Throws(ListExpected::class)
    fun vector(arguments: IList): Any =
        ScmVector.create(arguments)

    @Throws(VectorExpected::class)
    fun vector_2D_3Elist(argument: Any?): Any =
        toScmVector(argument).list

    @Throws(ListExpected::class)
    fun list_2D_3Evector(argument: Any?): Any =
        ScmVector.create(toList(argument))

    // 6.4 Control features
    fun procedure_3F(argument: Any?): Any = // procedure?
        ValueTraits.toScmBoolean(isFunction(argument))

    val apply: Function =
        ApplyFunction

    val call_2Dwith_2Dcurrent_2Dcontinuation: Function =
        CallCCFunction

    //  public final static Function dynamic_2Dwind =
    // DynamicWindFunction.INSTANCE;
    // 6.5 Eval
    val eval: Function =
        EvalFunction

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun scheme_2Dreport_2Denvironment(fst: Any?): Any {
        if (toScmNumber(fst).integer != 5) {
            throw SchemeRuntimeError(fst)
        }

        return getSchemeReportEnvironment()
    }

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun null_2Denvironment(fst: Any?): Any {
        if (toScmNumber(fst).integer != 5) {
            throw SchemeRuntimeError(fst)
        }

        return getNullEnvironment()
    }

    // 6.6 Input and output
    // 6.6.1 Ports
    fun port_3F(argument: Any?): Any = // port?
        ValueTraits.toScmBoolean(isPort(argument))

    @Throws(PortExpected::class)
    fun input_2Dport_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(argument is InputPort)

    @Throws(PortExpected::class)
    fun output_2Dport_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(argument is OutputPort)

    @Throws(StringExpected::class, OpenException::class)
    fun open_2Dinput_2Dfile(argument: Any?): Any =
        InputPort.create(toScmString(argument))

    @Throws(StringExpected::class, OpenException::class)
    fun open_2Doutput_2Dfile(argument: Any?): Any =
        OutputPort.create(toScmString(argument))

    @Throws(PortExpected::class, CloseException::class)
    fun close_2Dinput_2Dport(argument: Any?): Any? {
        toInputPort(argument).close()
        return argument
    }

    @Throws(PortExpected::class, CloseException::class)
    fun close_2Doutput_2Dport(argument: Any?): Any? {
        toOutputPort(argument).close()
        return argument
    }

    // 6.6.2 Input
    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun read(fst: Any?): Any? =
        try {
            toInputPort(fst).read()
        } catch (e: InterruptedException) {
            throw SchemeRuntimeError(fst, e.toString())
        }

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun read_2Dchar(fst: Any?): Any? =
        toInputPort(fst).readScmChar()

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun peek_2Dchar(fst: Any?): Any? =
        toInputPort(fst).peekScmChar()

    fun eof_2Dobject_3F(fst: Any?): Any =
        ValueTraits.toScmBoolean(eq(fst, InputPort.EOF_VALUE))

    @Throws(TypeError::class)
    fun char_2Dready_3F(fst: Any?): Any =
        ValueTraits.toScmBoolean(toInputPort(fst).isReady)

    // 6.6.3 Output
    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun write(fst: Any?, snd: Any?): Any? {
        toOutputPort(snd).write(fst)
        return snd
    }

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun display(fst: Any?, snd: Any?): Any? {
        toOutputPort(snd).display(fst)
        return snd
    }

    @Throws(SchemeRuntimeError::class, TypeError::class)
    fun write_2Dchar(fst: Any?, snd: Any?): Any? {
        toOutputPort(snd).writeScmChar(toScmChar(fst))
        return snd
    }

    // additional functions
    fun __unique_2Did(): Any =
        createUniqueSymbol()

    val __spawn: Function =
        SpawnFunction

    val __y_2Dcombinator: Function =
        YCombinator

    @Throws(TypeError::class)
    fun __open_2Dinput_2Dstring(argument: Any?): Any =
        InputPort.create(StringReader(toScmString(argument).javaString))

    //  not very usefull yet ... needs GET-OUTPUT-STRING
    //  public final static Object __open_2Doutput_2Dstring()
    //      throws TypeError
    //  {
    //      return OutputPort.create(
    //          new StringWriter()
    //      );
    //  }
}


fun prop(p: Function): Function =
    p

fun func(f: () -> Any?): Function =
    object : ValueThunk() {
        override fun checkedCall(): Any? = f()
    }

fun funcA(f: (Any?) -> Any?): Function =
    object : UnaryValueFunction() {
        override fun checkedCall(argument: Any?): Any? = f(argument)
    }

fun funcL(f: (IList) -> Any?): Function =
    object : Function() {
        override fun call(
            state: Registers, arguments: IList
        ): Any? = f(arguments)
    }


fun funcAA(f: (Any?, Any?) -> Any?): Function =
    object : BinaryValueFunction() {
        override fun checkedCall(
            fst: Any?, snd: Any?
        ): Any? = f(fst, snd)
    }

fun funcAAA(f: (Any?, Any?, Any?) -> Any?): Function =
    object : TernaryValueFunction() {
        override fun checkedCall(
            fst: Any?, snd: Any?, trd: Any?
        ): Any? = f(fst, snd, trd)
    }
