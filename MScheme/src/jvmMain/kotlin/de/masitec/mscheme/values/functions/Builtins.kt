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
package de.masitec.mscheme.values.functions

import de.masitec.mscheme.environment.Environment
import de.masitec.mscheme.exceptions.*
import de.masitec.mscheme.machine.Registers
import de.masitec.mscheme.values.*
import de.masitec.mscheme.values.Function
import de.masitec.mscheme.values.ListFactory
import de.masitec.mscheme.values.ScmString
import de.masitec.mscheme.values.ValueTraits
import de.masitec.mscheme.values.functions.Order
import java.io.StringReader


object Builtins {
    // 6. Standard procedures
    // 6.1 Equivalence predicates
    fun eq_3F(fst: Any?, snd: Any?): Any =
        ValueTraits.toScmBoolean(ValueTraits.eq(fst, snd))

    fun eqv_3F(fst: Any?, snd: Any?): Any =
        ValueTraits.toScmBoolean(ValueTraits.eqv(fst, snd))

    fun equal_3F(fst: Any?, snd: Any?): Any =
        ValueTraits.toScmBoolean(ValueTraits.equal(fst, snd))

    // 6.2 Numbers
    // 6.2.5 Numerical operations
    fun number_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument))

    fun complex_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument))

    fun real_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument))

    fun rational_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument))

    fun integer_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument))

    fun exact_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument))

    fun inexact_3F(argument: Any?): Any =
        ValueTraits.FALSE

    fun _3C(arguments: IList): Any =
        ValueTraits.toScmBoolean(Order.check(arguments, Order.LT))

    fun _3C_3D(arguments: IList): Any =
        ValueTraits.toScmBoolean(Order.check(arguments, Order.LE))

    fun _3D(arguments: IList): Any =
        ValueTraits.toScmBoolean(Order.check(arguments, Order.EQ))

    fun _3E_3D(arguments: IList): Any =
        ValueTraits.toScmBoolean(Order.check(arguments, Order.GE))

    fun _3E(arguments: IList): Any =
        ValueTraits.toScmBoolean(Order.check(arguments, Order.GT))

    fun zero_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(
            ValueTraits.toScmNumber(argument).integer == 0
        )

    fun _2B(arguments: IList): Any {
        var sum = ScmNumber.create(0)
        var tail = arguments

        while (!tail.isEmpty) {
            val term = ValueTraits.toScmNumber(tail.head)
            val nextTail = tail.tail

            sum = sum.plus(term)
            tail = nextTail
        }

        return sum
    }

    fun _2D(arguments: IList): Any {
        var result = ValueTraits.toScmNumber(arguments.head)
        var rest = arguments.tail

        if (!rest.isEmpty) {
            do {
                val head = ValueTraits.toScmNumber(rest.head)
                val tail = rest.tail

                result = result.minus(head)
                rest = tail
            } while (!rest.isEmpty)

            return result
        } else {
            return result.negated()
        }
    }

    fun _2A(arguments: IList): Any {
        var product = ScmNumber.create(1)
        var tail = arguments

        while (!tail.isEmpty) {
            val factor = ValueTraits.toScmNumber(tail.head)
            val nextTail = tail.tail

            product = product.times(factor)
            tail = nextTail
        }

        return product
    }

    fun _2F(arguments: IList): Any {
        var result = ValueTraits.toScmNumber(arguments.head)
        var rest = arguments.tail

        if (!rest.isEmpty) {
            do {
                val head = ValueTraits.toScmNumber(rest.head)
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
        ValueTraits.toScmBoolean(!ValueTraits.isTrue(argument))

    fun boolean_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(ValueTraits.isScmBoolean(argument))

    // 6.3.2 Pairs and lists
    fun pair_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(ValueTraits.isPair(argument))

    fun cons(fst: Any?, snd: Any?): Any =
        ListFactory.createPair(fst, snd)

    fun car(argument: Any?): Any? =
        ValueTraits.toConstPair(argument).first

    fun cdr(argument: Any?): Any? =
        ValueTraits.toConstPair(argument).second

    fun set_2Dcar_21(fst: Any?, snd: Any?): Any? {
        ValueTraits.toMutablePair(fst).first = snd
        return snd
    }

    fun set_2Dcdr_21(fst: Any?, snd: Any?): Any? {
        ValueTraits.toMutablePair(fst).second = snd
        return snd
    }

    fun null_3F(argument: Any?) = // null?
        ValueTraits.toScmBoolean(argument is Empty)

    fun list_3F(argument: Any?): Any = // list?
        ValueTraits.toScmBoolean(ValueTraits.isList(argument))

    fun list(argument: IList): Any {
        // Without first-class continuations, it would be safe to
        // omit the call to getCopy(). But multiple returns have to
        // return different lists ...
        return argument.getCopy()
    }

    fun length(argument: Any?): Any =
        ScmNumber.create(ValueTraits.toList(argument).length)

    val append: Function =
        AppendFunction

    fun reverse(argument: Any?): Any =
        ValueTraits.toList(argument).getReversed()

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
        ValueTraits.toScmBoolean(ValueTraits.isSymbol(argument))

    fun symbol_2D_3Estring(argument: Any?): Any =
        ScmString.createConst(ValueTraits.toSymbol(argument))

    fun string_2D_3Esymbol(argument: Any?): Any =
        ValueTraits.toScmString(argument).javaString

    // 6.3.4 Characters
    fun char_3F(argument: Any?) = // char?
        ValueTraits.toScmBoolean(ValueTraits.isScmChar(argument))

    fun char_3C_3F(fst: Any?, snd: Any?): Any =
        ValueTraits.toScmBoolean(
            ValueTraits.toScmChar(fst) < ValueTraits.toScmChar(
                snd
            )
        )

    fun char_3C_3D_3F(fst: Any?, snd: Any?): Any {
        return ValueTraits.toScmBoolean(
            ValueTraits.toScmChar(fst) <= ValueTraits.toScmChar(snd)
        )
    }

    fun char_3D_3F(fst: Any?, snd: Any?): Any =
        ValueTraits.toScmBoolean(
            ValueTraits.toScmChar(fst) == ValueTraits.toScmChar(snd)
        )

    fun char_3E_3D_3F(fst: Any?, snd: Any?): Any =
        ValueTraits.toScmBoolean(
            ValueTraits.toScmChar(fst) >= ValueTraits.toScmChar(snd)
        )

    fun char_3E_3F(fst: Any?, snd: Any?): Any =
        ValueTraits.toScmBoolean(
            ValueTraits.toScmChar(fst) > ValueTraits.toScmChar(snd)
        )

    fun char_2D_3Einteger(argument: Any?): Any =
        ScmNumber.create(ValueTraits.toScmChar(argument).code)

    fun integer_2D_3Echar(argument: Any?): Any =
        ValueTraits.toScmChar(
            ValueTraits.toScmNumber(argument).integer.toChar()
        )

    fun char_2Dupcase(argument: Any?): Any =
        ValueTraits.toScmChar(ValueTraits.toScmChar(argument).uppercaseChar())

    fun char_2Ddowncase(argument: Any?): Any =
        ValueTraits.toScmChar(ValueTraits.toScmChar(argument).lowercaseChar())

    // 6.3.5 Strings
    fun string_3F(argument: Any?): Any = // string?
        ValueTraits.toScmBoolean(ValueTraits.isScmString(argument))

    fun make_2Dstring(k: Any?, c: Any?): Any =
        ScmString.create(
            ValueTraits.toScmNumber(k).integer,
            ValueTraits.toScmChar(c)
        )

    fun string_2Dlength(str: Any?): Any =
        ScmNumber.create(ValueTraits.toScmString(str).length)

    fun string_2Dref(str: Any?, k: Any?): Any =
        ValueTraits.toScmChar(
            ValueTraits.toScmString(str).get(
                ValueTraits.toScmNumber(k).integer
            )
        )

    fun string_2Dset_21(str: Any?, k: Any?, c: Any?): Any? {
        ValueTraits.toScmString(str).set(
            ValueTraits.toScmNumber(k).integer,
            ValueTraits.toScmChar(c)
        )

        return c
    }

    fun string_2Dappend(arguments: IList): Any {
        val accu = StringBuilder()

        var rest = arguments
        while (!rest.isEmpty) {
            accu.append(
                ValueTraits.toScmString(rest.head).javaString
            )
            rest = rest.tail
        }

        return ScmString.create(accu.toString())
    }

    fun string_2Dcopy(string: Any?): Any =
        ScmString.create(ValueTraits.toScmString(string).javaString)

    fun string_2D_3Elist(scmString: Any?): Any {
        val javaString = ValueTraits.toScmString(scmString).javaString
        var result = ListFactory.create()

        for (i in javaString.length - 1 downTo 0) {
            result = ListFactory.prepend(
                ValueTraits.toScmChar(javaString[i]),
                result
            )
        }

        return result
    }

    fun list_2D_3Estring(list: Any?): Any {
        val accu = StringBuilder()

        var rest = ValueTraits.toList(list)
        while (!rest.isEmpty) {
            accu.append(ValueTraits.toScmChar(rest.head))
            rest = rest.tail
        }

        return ScmString.create(accu.toString())
    }

    // 6.3.6 Vectors
    fun vector_3F(argument: Any?) = // vector?
        ValueTraits.toScmBoolean(ValueTraits.isScmVector(argument))

    fun make_2Dvector(k: Any?, obj: Any?): Any =
        ScmVector.create(ValueTraits.toScmNumber(k).integer, obj)

    fun vector_2Dlength(str: Any?): Any =
        ScmNumber.create(ValueTraits.toScmVector(str).length)

    fun vector_2Dref(vector: Any?, k: Any?): Any? =
        ValueTraits.toScmVector(vector).get(
            ValueTraits.toScmNumber(k).integer
        )

    fun vector_2Dset_21(vector: Any?, k: Any?, obj: Any?): Any? {
        ValueTraits.toScmVector(vector).set(
            ValueTraits.toScmNumber(k).integer, obj
        )

        return obj
    }

    fun vector(arguments: IList): Any =
        ScmVector.create(arguments)

    fun vector_2D_3Elist(argument: Any?): Any =
        ValueTraits.toScmVector(argument).list

    fun list_2D_3Evector(argument: Any?): Any =
        ScmVector.create(ValueTraits.toList(argument))

    // 6.4 Control features
    fun procedure_3F(argument: Any?): Any = // procedure?
        ValueTraits.toScmBoolean(ValueTraits.isFunction(argument))

    val apply: Function =
        ApplyFunction

    val call_2Dwith_2Dcurrent_2Dcontinuation: Function =
        CallCCFunction

    //  public final static Function dynamic_2Dwind =
    // DynamicWindFunction.INSTANCE;
    // 6.5 Eval
    val eval: Function =
        EvalFunction

    fun scheme_2Dreport_2Denvironment(fst: Any?): Any {
        if (ValueTraits.toScmNumber(fst).integer != 5) {
            throw SchemeRuntimeError(fst)
        }

        return Environment.getSchemeReportEnvironment()
    }

    fun null_2Denvironment(fst: Any?): Any {
        if (ValueTraits.toScmNumber(fst).integer != 5) {
            throw SchemeRuntimeError(fst)
        }

        return Environment.getNullEnvironment()
    }

    // 6.6 Input and output
    // 6.6.1 Ports
    fun port_3F(argument: Any?): Any = // port?
        ValueTraits.toScmBoolean(ValueTraits.isPort(argument))

    fun input_2Dport_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(argument is InputPort)

    fun output_2Dport_3F(argument: Any?): Any =
        ValueTraits.toScmBoolean(argument is OutputPort)

    fun open_2Dinput_2Dfile(argument: Any?): Any =
        InputPort.create(ValueTraits.toScmString(argument))

    fun open_2Doutput_2Dfile(argument: Any?): Any =
        OutputPort.create(ValueTraits.toScmString(argument))

    fun close_2Dinput_2Dport(argument: Any?): Any? {
        ValueTraits.toInputPort(argument).close()
        return argument
    }

    fun close_2Doutput_2Dport(argument: Any?): Any? {
        ValueTraits.toOutputPort(argument).close()
        return argument
    }

    // 6.6.2 Input
    fun read(fst: Any?): Any? =
        try {
            ValueTraits.toInputPort(fst).read()
        } catch (e: InterruptedException) {
            throw SchemeRuntimeError(fst, e.toString())
        }

    fun read_2Dchar(fst: Any?): Any? =
        ValueTraits.toInputPort(fst).readScmChar()

    fun peek_2Dchar(fst: Any?): Any? =
        ValueTraits.toInputPort(fst).peekScmChar()

    fun eof_2Dobject_3F(fst: Any?): Any =
        ValueTraits.toScmBoolean(ValueTraits.eq(fst, InputPort.EOF_VALUE))

    fun char_2Dready_3F(fst: Any?): Any =
        ValueTraits.toScmBoolean(ValueTraits.toInputPort(fst).isReady)

    // 6.6.3 Output
    fun write(fst: Any?, snd: Any?): Any? {
        ValueTraits.toOutputPort(snd).write(fst)
        return snd
    }

    fun display(fst: Any?, snd: Any?): Any? {
        ValueTraits.toOutputPort(snd).display(fst)
        return snd
    }

    fun write_2Dchar(fst: Any?, snd: Any?): Any? {
        ValueTraits.toOutputPort(snd).writeScmChar(ValueTraits.toScmChar(fst))
        return snd
    }

    // additional functions
    fun __unique_2Did(): Any =
        ValueTraits.createUniqueSymbol()

    val __spawn: Function =
        SpawnFunction

    val __y_2Dcombinator: Function =
        YCombinator

    fun __open_2Dinput_2Dstring(argument: Any?): Any =
        InputPort.create(StringReader(ValueTraits.toScmString(argument).javaString))

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
