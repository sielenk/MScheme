/*
 * The 'virtual scheme machine'. Copyright (C) 2001 Marvin H. Sielenkemper
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

package mscheme.machine;

import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;

import mscheme.IInit;
import mscheme.code.IReduceable;
import mscheme.compiler.Compiler;
import mscheme.environment.DynamicEnvironment;
import mscheme.environment.Environment;
import mscheme.exceptions.RuntimeError;
import mscheme.exceptions.SchemeException;
import mscheme.exceptions.TypeError;
import mscheme.values.Function;
import mscheme.values.IList;
import mscheme.values.InputPort;
import mscheme.values.ListFactory;
import mscheme.values.OutputPort;
import mscheme.values.ScmNumber;
import mscheme.values.ValueTraits;
import mscheme.values.functions.UnaryValueFunction;
import mscheme.values.functions.ValueThunk;

public final class Machine
        implements Runnable
{
    public final static String CVS_ID = "$Id$";

    private final Environment _environment;

    private InputPort _stdin;

    private OutputPort _stdout;

    private Function _errorHandler = null;

    private int _tickerRed = 0;

    private int _tickerInv = 0;

    public Machine()
    {
        _environment = Environment.getSchemeReportEnvironment();
        _stdin = InputPort.create(new InputStreamReader(System.in));
        _stdout = OutputPort.create(new OutputStreamWriter(System.out));
        init();

        // I would call this(...) but it kills gcj 3.0.2 ...
    }

    public Machine(Reader stdin, Writer stdout)
    {
        _environment = Environment.getSchemeReportEnvironment();
        _stdin = InputPort.create(stdin);
        _stdout = OutputPort.create(stdout);
        init();
    }

    public Machine(Environment environment)
    {
        _environment = environment;
        _stdin = InputPort.create(new InputStreamReader(System.in));
        _stdout = OutputPort.create(new OutputStreamWriter(System.out));
    }

    private void init()
    {
        try
        {
            _environment.define("current-input-port",
                    new ValueThunk()
                    {
                        protected Object checkedCall()
                        {
                            return _stdin;
                        }
                    });

            _environment.define("reset-input-port",
                    new UnaryValueFunction()
                    {
                        protected Object checkedCall(Object argument)
                                throws TypeError
                        {
                            Object result = _stdin;
                            _stdin = (InputPort) argument;
                            return result;
                        }
                    });

            _environment.define("current-output-port",
                    new ValueThunk()
                    {
                        protected Object checkedCall()
                        {
                            return _stdout;
                        }
                    });

            _environment.define("reset-output-port",
                    new UnaryValueFunction()
                    {
                        protected Object checkedCall(Object argument)
                                throws TypeError
                        {
                            Object result = _stdout;
                            _stdout = (OutputPort) argument;
                            return result;
                        }
                    });

            _environment.define("current-error-handler",
                    new ValueThunk()
                    {
                        protected Object checkedCall()
                        {
                            if (_errorHandler != null)
                            {
                                return _errorHandler;
                            }
                            return Boolean.FALSE;
                        }
                    });

            _environment.define("reset-error-handler",
                    new UnaryValueFunction()
                    {
                        protected Object checkedCall(Object argument)
                                throws TypeError
                        {
                            Object oldErrorHandler = _errorHandler;

                            _errorHandler = ValueTraits.isTrue(argument) ? (Function) argument
                                    : null;

                            if (oldErrorHandler != null)
                            {
                                return oldErrorHandler;
                            }

                            return Boolean.FALSE;
                        }
                    });

            _environment.define("machine-environment",
                    _environment);

            _environment.define(
                    "ticker",
                    new ValueThunk()
                    {
                        protected Object checkedCall()
                        {
                            int tRed = _tickerRed;
                            int tInv = _tickerInv;
                            _tickerRed = _tickerInv = 0;
                            return ListFactory.create(ScmNumber.create(tRed), ScmNumber
                                    .create(tInv));
                        }
                    });

            evaluate(InputPort.create(new StringReader(IInit.BOOTSTRAP)).read());
        }
        catch (SchemeException e)
        {
            throw new RuntimeException(e.toString());
        }
        catch (InterruptedException e)
        {
            throw new RuntimeException(e.toString());
        }
    }

    public Object unprotectedRun()
            throws SchemeException, InterruptedException
    {
        return evaluate(InputPort.create(new StringReader(IInit.REP)).read());
    }

    public void run()
    {
        try
        {
            unprotectedRun();
        }
        catch (Throwable t)
        {
            System.err.println(t.toString());
        }
    }

    public Environment getEnvironment()
    {
        return _environment;
    }

    class Executor
    {
        private boolean _done;

        private Registers _state;

        Executor(DynamicEnvironment environment)
        {
            _state = new Registers(environment);
        }

        public Object execute(Object expression)
                throws SchemeException, InterruptedException
        {
            Object current = expression;

            _done = false;
            while (!_done)
            {
                try
                {
                    if (current instanceof IReduceable)
                    {
                        current = ((IReduceable) current).reduce(_state);
                        ++_tickerRed;
                    }
                    else
                    {
                        current = invoke(current);
                        ++_tickerInv;
                    }
                }
                catch (SchemeException error)
                {
                    current = handleError(error);
                }
            }

            return current;
        }

        private Object invoke(Object current)
                throws SchemeException, InterruptedException
        {
            IStack stack = _state.getStack();

            if (!stack.isEmpty())
            {
                final StackFrame frame = stack.pop();

                _state.setEnvironment(frame.environment);

                current = frame.invokeable.invoke(_state, current);
            }
            else
            {
                _done = true;
            }

            return current;
        }

        private Object handleError(SchemeException error)
                throws RuntimeError, SchemeException, InterruptedException
        {
            if (_errorHandler != null)
            {
                IList errorValue = ListFactory.create(error.getCauseValue(),
                        error.getMessage(), _state.getCurrentContinuation(),
                        Boolean.valueOf(error instanceof RuntimeError));

                // Avoid endless loop if the
                // handler is buggy:
                Function handler = _errorHandler;
                _errorHandler = null;

                return ValueTraits.apply(_state, handler, ListFactory
                        .create(errorValue));
            }
            else
            {
                throw error;
            }
        }
    }

    public Object execute(Object expression)
            throws SchemeException, InterruptedException
    {
        return new Executor(getEnvironment().getDynamic()).execute(expression);
    }

    public Object compile(Object compilee)
            throws SchemeException, InterruptedException
    {
        return new Compiler(getEnvironment().getStatic()).compile(compilee);
    }

    public static Object parse(String expression)
            throws SchemeException, InterruptedException
    {
        return InputPort.create(new StringReader(expression)).read();
    }

    public Object evaluate(Object evaluatee)
            throws SchemeException, InterruptedException
    {
        return execute(compile(evaluatee));
    }

    public Object evaluate(String expression)
            throws SchemeException, InterruptedException
    {
        return evaluate(parse(expression));
    }
}