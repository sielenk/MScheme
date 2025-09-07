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

/*
 * Created on 02.01.2004
 *
 */
package de.masitec.mscheme.values

import de.masitec.mscheme.environment.Environment
import de.masitec.mscheme.environment.StaticEnvironment
import de.masitec.mscheme.exceptions.*
import de.masitec.mscheme.machine.Registers
import de.masitec.mscheme.util.Arity
import java.io.IOException
import java.io.Writer
import java.lang.reflect.Field
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import kotlin.reflect.KCallable
import kotlin.reflect.KClass
import kotlin.reflect.KFunction
import kotlin.reflect.cast

/**
 * @author sielenk
 */
object ValueTraits {
    const val TRUE: Boolean = true

    const val FALSE: Boolean = false

    fun isTrue(o: Any?): Boolean =
        o != false

    fun isEmpty(obj: Any?): Boolean =
        obj === Empty

    fun apply(state: Registers, function: Any?, arguments: IList): Any? {
        when (function) {
            is KCallable<*> -> {
                val types = function.parameters
                    .map { it.type.classifier as KClass<*> }
                val hasTailParam = types.lastOrNull() == IList::class
                val fixCount = types.size - if (hasTailParam) 1 else 0
                var restOpt: IList? = arguments
                val argumentArray = types.map { type ->
                    val rest = restOpt ?: throw RuntimeArityError(
                        arguments,
                        if (hasTailParam)
                            Arity.atLeast(fixCount)
                        else
                            Arity.exactly(fixCount)
                    )

                    if (type == IList::class) {
                        restOpt = null
                        rest
                    } else {
                        restOpt = rest.tail
                        type.cast(rest.head)
                    }
                }.toTypedArray()

                try {
                    return function.call(*argumentArray)
                } catch (e1: IllegalArgumentException) {
                    throw SchemeRuntimeError(function, e1.toString())
                } catch (e1: IllegalAccessException) {
                    throw SchemeRuntimeError(function, e1.toString())
                } catch (e1: InvocationTargetException) {
                    throw SchemeRuntimeError(function, e1.toString())
                }
            }

            is Method -> {
                val parameterTypes = function.parameterTypes
                val methodExpectsIList = ((parameterTypes.size == 1) &&
                        (parameterTypes[0] == IList::class.java))

                return try {
                    if (arguments.isEmpty) {
                        if (methodExpectsIList) {
                            function.invoke(null, arguments)
                        } else {
                            function.invoke(null)
                        }
                    } else if (Modifier.isStatic(function.modifiers)) {
                        if (methodExpectsIList) {
                            function.invoke(null, arguments)
                        } else {
                            function.invoke(null, *arguments.getArray())
                        }
                    } else {
                        if (methodExpectsIList) {
                            function.invoke(arguments.head, arguments.tail)
                        } else {
                            function.invoke(
                                arguments.head,
                                *arguments.tail.getArray()
                            )
                        }
                    }
                } catch (e1: IllegalArgumentException) {
                    throw SchemeRuntimeError(function, e1.toString())
                } catch (e1: IllegalAccessException) {
                    throw SchemeRuntimeError(function, e1.toString())
                } catch (e1: InvocationTargetException) {
                    throw SchemeRuntimeError(function, e1.toString())
                }
            }

            is Field -> {
                try {
                    if (Modifier.isStatic(function.modifiers)) {
                        if (!arguments.isEmpty) {
                            throw RuntimeArityError(arguments, Arity.exactly(0))
                        }

                        return function.get(null)
                    } else {
                        if (!arguments.tail.isEmpty) {
                            throw RuntimeArityError(arguments, Arity.exactly(1))
                        }

                        return function.get(arguments.head)
                    }
                } catch (e: IllegalArgumentException) {
                    throw SchemeRuntimeError(function, e.toString())
                } catch (e: IllegalAccessException) {
                    throw SchemeRuntimeError(function, e.toString())
                }
            }

            is Function -> {
                return function.call(state, arguments)
            }

            else -> {
                throw FunctionExpected(function)
            }
        }
    }

    fun eq(fst: Any?, snd: Any?): Boolean =
        if (fst is IComparable)
            fst.eq(snd)
        else
            fst === snd

    fun eqv(fst: Any?, snd: Any?): Boolean =
        if (fst is Char)
            fst == snd
        else if (fst is IComparable)
            fst.eqv(snd)
        else
            fst === snd


    fun equal(fst: Any?, snd: Any?): Boolean =
        fst == snd

    fun isList(o: Any?): Boolean =
        o is IList && o.isValid

    fun toList(o: Any?): IList =
        if (o is IList)
            o.validate()
        else
            throw ListExpected(o)

    fun isPair(o: Any?): Boolean =
        o is IPair

    fun toConstPair(o: Any?): IPair {
        if (o is IPair) {
            return o
        } else {
            throw PairExpected(o)
        }
    }

    fun toMutablePair(o: Any?): IMutablePair =
        when (o) {
            is IMutablePair ->
                o

            !is IPair ->
                throw PairExpected(o)

            else ->
                throw ImmutableException(o)
        }


    fun toInputPort(o: Any?): InputPort =
        o as? InputPort ?: throw InputPortExpected(o)

    fun toSymbol(o: Any?): String =
        o as? String ?: throw SymbolExpected(o)

    private var _index = 0

    fun createUniqueSymbol(): String {
        return "#[" + _index++ + "]"
    }

    fun toScmBoolean(b: Boolean): Boolean =
        b

    fun toScmBoolean(o: Any?): Boolean =
        isTrue(o)

    fun toScmNumber(o: Any?): ScmNumber =
        o as? ScmNumber ?: throw NumberExpected(o)

    fun toScmNumber(i: Int): Any =
        ScmNumber.create(i)

    fun toScmChar(o: Any?): Char =
        o as? Char ?: throw CharExpected(o)

    fun toScmChar(c: Char): Char =
        c

    fun toScmString(o: Any?): ScmString =
        o as? ScmString ?: throw StringExpected(o)

    fun toScmVector(o: Any?): ScmVector =
        o as? ScmVector ?: throw VectorExpected(o)

    fun toOutputPort(o: Any?): OutputPort =
        o as? OutputPort ?: throw OutputPortExpected(o)

    fun toEnvironment(o: Any?): Environment =
        o as? Environment ?: throw EnvironmentExpected(o)

    fun toStaticEnvironment(o: Any?): StaticEnvironment =
        o as? StaticEnvironment ?: throw EnvironmentExpected(o)

    fun isScmBoolean(o: Any?): Boolean =
        o is Boolean

    fun isSymbol(o: Any?): Boolean =
        o is String

    fun isScmNumber(o: Any?): Boolean =
        o is ScmNumber

    fun isScmChar(o: Any?): Boolean =
        o is Char

    fun isScmString(o: Any?): Boolean =
        o is ScmString

    fun isScmVector(o: Any?): Boolean =
        o is ScmVector

    fun isPort(o: Any?): Boolean =
        o is Port

    fun isFunction(o: Any?): Boolean =
        o is Function || o is KCallable<*> || o is Method || o is Field

    fun output(destination: Writer, doWrite: Boolean, o: Any?) {
        if (o is Char) {
            if (doWrite) {
                destination.write("#\\")

                when (o) {
                    ' ' -> destination.write("space")
                    '\n' -> destination.write("newline")
                    else -> destination.write(o.code)
                }
            } else {
                destination.write(o.code)
            }
        } else if (isScmBoolean(o)) {
            destination.write(if (isTrue(o)) "#t" else "#f")
        } else if (o is IOutputable) {
            o.outputOn(destination, doWrite)
        } else {
            if (doWrite) {
                destination.write("#[$o]")
            } else {
                destination.write(o.toString())
            }
        }
    }

    fun display(destination: Writer, o: Any?) {
        output(destination, false, o)
    }

    fun write(destination: Writer, o: Any?) {
        output(destination, true, o)
    }

    fun getConst(o: Any?): Any? =
        if (o is IMutable)
            o.getConst()
        else
            o
}
