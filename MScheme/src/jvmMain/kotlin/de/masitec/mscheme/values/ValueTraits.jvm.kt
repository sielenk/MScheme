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

package de.masitec.mscheme.values

import de.masitec.mscheme.exceptions.FunctionExpected
import de.masitec.mscheme.exceptions.RuntimeArityError
import de.masitec.mscheme.exceptions.SchemeRuntimeError
import de.masitec.mscheme.machine.Registers
import de.masitec.mscheme.util.Arity
import java.lang.reflect.Field
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import kotlin.reflect.KCallable
import kotlin.reflect.KClass
import kotlin.reflect.cast


actual fun mpIsFunction(o: Any?): Boolean =
    o is Function || o is KCallable<*> || o is Method || o is Field

actual fun mpApply(
    state: Registers,
    function: Any?,
    arguments: IList
): Any? {
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
