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
package de.masitec.mscheme.machine

import de.masitec.mscheme.Init
import de.masitec.mscheme.code.IReduceable
import de.masitec.mscheme.compiler.Compiler
import de.masitec.mscheme.environment.DynamicEnvironment
import de.masitec.mscheme.environment.Environment
import de.masitec.mscheme.exceptions.SchemeRuntimeError
import de.masitec.mscheme.exceptions.SchemeException
import de.masitec.mscheme.values.*
import de.masitec.mscheme.values.Function
import de.masitec.mscheme.values.functions.Subcontinuation
import de.masitec.mscheme.values.functions.UnaryValueFunction
import de.masitec.mscheme.values.functions.ValueThunk
import java.io.*
import java.lang.Boolean
import kotlin.Any
import kotlin.RuntimeException
import kotlin.String
import kotlin.Throwable
import kotlin.toString

class Machine : Runnable {
    private var _stdin: InputPort?
    private var _stdout: OutputPort?
    private var _errorHandler: Function? = null
    private var _tickerRed = 0
    private var _tickerInv = 0

    val environment: Environment

    constructor() {
        this.environment = Environment.getSchemeReportEnvironment()
        _stdin = InputPort.create(InputStreamReader(System.`in`))
        _stdout = OutputPort.create(OutputStreamWriter(System.out))
        init()

        // I would call this(...) but it kills gcj 3.0.2 ...
    }

    constructor(stdin: Reader, stdout: Writer) {
        this.environment = Environment.getSchemeReportEnvironment()
        _stdin = InputPort.create(stdin)
        _stdout = OutputPort.create(stdout)
        init()
    }

    constructor(environment: Environment) {
        this.environment = environment
        _stdin = InputPort.create(InputStreamReader(System.`in`))
        _stdout = OutputPort.create(OutputStreamWriter(System.out))
    }

    private fun init() {
        try {
            environment.define(
                "current-input-port",
                object : ValueThunk() {
                    override fun checkedCall(): Any? =
                        _stdin
                }
            )

            environment.define(
                "reset-input-port",
                object : UnaryValueFunction() {
                    override fun checkedCall(argument: Any?): Any? {
                        val result: Any? = _stdin
                        _stdin = argument as InputPort?
                        return result
                    }
                }
            )

            environment.define(
                "current-output-port",
                object : ValueThunk() {
                    override fun checkedCall(): Any? =
                        _stdout
                })

            environment.define(
                "reset-output-port",
                object : UnaryValueFunction() {
                    override fun checkedCall(argument: Any?): Any? {
                        val result: Any? = _stdout
                        _stdout = argument as OutputPort?
                        return result
                    }
                }
            )

            environment.define(
                "current-error-handler",
                object : ValueThunk() {
                    override fun checkedCall(): Any =
                        _errorHandler ?: Boolean.FALSE
                }
            )

            environment.define(
                "reset-error-handler",
                object : UnaryValueFunction() {
                    override fun checkedCall(argument: Any?): Any {
                        val oldErrorHandler: Any? = _errorHandler

                        _errorHandler = if (ValueTraits.isTrue(argument))
                            argument as Function?
                        else
                            null

                        return oldErrorHandler ?: Boolean.FALSE
                    }
                }
            )

            environment.define(
                "machine-environment",
                this.environment
            )

            environment.define(
                "ticker",
                object : ValueThunk() {
                    override fun checkedCall(): Any {
                        val tRed = _tickerRed
                        val tInv = _tickerInv
                        _tickerInv = 0
                        _tickerRed = _tickerInv

                        return ListFactory.create(
                            ScmNumber.create(tRed), ScmNumber
                                .create(tInv)
                        )
                    }
                }
            )

            evaluate(InputPort.create(StringReader(Init.BOOTSTRAP)).read())
        } catch (e: SchemeException) {
            throw RuntimeException(e.toString())
        } catch (e: InterruptedException) {
            throw RuntimeException(e.toString())
        }
    }

    fun unprotectedRun(): Any? =
        evaluate(InputPort.create(StringReader(Init.REP)).read())

    override fun run() {
        try {
            unprotectedRun()
        } catch (t: Throwable) {
            System.err.println(t)
        }
    }

    internal inner class Executor(environment: DynamicEnvironment) {
        private var _done = false
        private val _state: Registers =
            Registers(environment)

        fun execute(expression: Any?): Any? {
            var current = expression

            _done = false
            while (!_done) {
                try {
                    if (current is IReduceable) {
                        current = current.reduce(_state)
                        ++_tickerRed
                    } else {
                        current = invoke(current)
                        ++_tickerInv
                    }
                } catch (error: SchemeException) {
                    current = handleError(error)
                }
            }

            return current
        }

        private fun invoke(current: Any?): Any? {
            var current = current
            val stack = _state.stack

            if (!stack.isEmpty) {
                val frame = stack.pop()

                _state.environment = frame.environment!!

                current = frame.continuation!!.invoke(_state, current)
            } else {
                _done = true
            }

            return current
        }

        private fun handleError(error: SchemeException): Any? {
            if (_errorHandler != null) {
                val errorValue = ListFactory.create(
                    error.causeValue,
                    error.message,
                    Subcontinuation(
                        _state.stack.continuation
                    ),
                    error is SchemeRuntimeError
                )

                // Avoid endless loop if the
                // handler is buggy:
                val handler = _errorHandler
                _errorHandler = null

                return ValueTraits.apply(
                    _state, handler, ListFactory
                        .create(errorValue)
                )
            } else {
                throw error
            }
        }
    }

    fun execute(expression: Any?): Any? =
        Executor(this.environment.dynamic).execute(expression)

    fun compile(compilee: Any?): Any? =
        Compiler(this.environment.static).compile(compilee)

    fun evaluate(evaluatee: Any?): Any? =
        execute(compile(evaluatee))

    fun evaluate(expression: String): Any? =
        evaluate(parse(expression))

    companion object {
        fun parse(expression: String): Any? =
            InputPort.create(StringReader(expression)).read()
    }
}
