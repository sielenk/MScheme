/* The 'virtual scheme machine'.
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

package mscheme.machine;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;

import mscheme.Code;
import mscheme.Init;
import mscheme.Value;

import mscheme.environment.Environment;
import mscheme.environment.StaticEnvironment;

import mscheme.exceptions.RuntimeError;
import mscheme.exceptions.SchemeException;
import mscheme.exceptions.TypeError;

import mscheme.code.Application;

import mscheme.values.InputPort;
import mscheme.values.ListFactory;
import mscheme.values.OutputPort;
import mscheme.values.ScmBoolean;
import mscheme.values.ScmNumber;
import mscheme.values.List;
import mscheme.values.Symbol;
import mscheme.values.Function;

import mscheme.values.functions.UnaryValueFunction;
import mscheme.values.functions.ValueThunk;


final class StopContinuation
    extends Continuation
{
    public final static String id
        = "$Id$";


    private Value _result;

    StopContinuation(Registers state)
    {
        super(state);
        _result = null;
    }

    Value getResult()
    {
        return _result;
    }

    boolean hasResult()
    {
        return _result != null;
    }

    protected Code executionStep(Registers state, Value evaluationResult)
    {
        _result = evaluationResult;
        return _result;
    }


    protected String debugString()
    {
        return "abort";
    }
}


public final class Machine
    implements Runnable
{
    public final static String id
        = "$Id$";


    private final Environment _environment;

    private InputPort  _stdin;
    private OutputPort _stdout;

    private Function   _errorHandler = null;

    private int        _ticker = 0;


    public Machine()
    {
        _environment = Environment.getSchemeReportEnvironment();
        _stdin       = InputPort .create(new InputStreamReader (System.in ));
        _stdout      = OutputPort.create(new OutputStreamWriter(System.out));
        init();
        
        // I would call this(...) but it kills gcj 3.0.2 ...
    }

    public Machine(
        Reader stdin,
        Writer stdout
    )
    {
        _environment = Environment.getSchemeReportEnvironment();
        _stdin       = InputPort .create(stdin );
        _stdout      = OutputPort.create(stdout);
        init();
    }

    public Machine(
        Environment environment
    )
    {
        _environment = environment;
        _stdin       = InputPort .create(new InputStreamReader (System.in ));
        _stdout      = OutputPort.create(new OutputStreamWriter(System.out));
    }


    private void init()
    {
        try
        {
            _environment.define(
                Symbol.create("current-input-port"),
                new ValueThunk()
                { protected Value checkedCall() { return _stdin; } }
            );

            _environment.define(
                Symbol.create("reset-input-port"),
                new UnaryValueFunction()
                {
                    protected Value checkedCall(Value argument)
                        throws TypeError
                    {
                        Value result = _stdin;
                        _stdin = argument.toInputPort();
                        return result;
                    }
                }
            );

            _environment.define(
                Symbol.create("current-output-port"),
                new ValueThunk()
                { protected Value checkedCall() { return _stdout; } }
            );

            _environment.define(
                Symbol.create("reset-output-port"),
                new UnaryValueFunction()
                {
                    protected Value checkedCall(Value argument)
                        throws TypeError
                    {
                        Value result = _stdout;
                        _stdout = argument.toOutputPort();
                        return result;
                    }
                }
            );

            _environment.define(
                Symbol.create("current-error-handler"),
                new ValueThunk()
                {
                    protected Value checkedCall()
                    {
                        if (_errorHandler != null)
                        {
                            return _errorHandler;
                        }
                        return ScmBoolean.createFalse();
                    }
                }
            );

            _environment.define(
                Symbol.create("reset-error-handler"),
                new UnaryValueFunction()
                {
                    protected Value checkedCall(Value argument)
                        throws TypeError
                    {
                        Value oldErrorHandler = _errorHandler;

                        _errorHandler =
                            argument.isTrue()
                            ? argument.toFunction()
                            : null;

                        if (oldErrorHandler != null)
                        {
                            return oldErrorHandler;
                        }
                        return ScmBoolean.createFalse();
                    }
                }
            );

            _environment.define(
                Symbol.create("machine-environment"),
                _environment
            );

            _environment.define(
                Symbol.create("ticker"),
                new ValueThunk()
                {
                    protected Value checkedCall() 
                    {
                        int t = _ticker;
                        _ticker = 0;
                        return ScmNumber.create(t);
                    }
                }
            );

            evaluate(
                InputPort.create(
                    new StringReader(
                        Init.bootstrap
                    )
                ).read()
            );
        }
        catch (SchemeException e)
        {
            throw new RuntimeException(
                e.toString()
            );
        }
    }

    public Value unprotectedRun()
        throws SchemeException
    {
        return evaluate(
            InputPort.create(
                new StringReader(
                    Init.rep
                )
            ).read()
        );
    }

    public void run()
    {
        try {
            unprotectedRun();
        }
        catch (Throwable t)
        {
            System.err.println(
                t.toString()
            );
        }
    }


    public Environment getEnvironment()
    {
        return _environment;
    }


    public Value execute(
        Code program
    ) throws SchemeException
    {
        Code             current = program;
        Registers        state   = new Registers(getEnvironment().getDynamic());
        StopContinuation stop    = new StopContinuation(state);

        while (!stop.hasResult())
        {
            try
            {
                current = current.executionStep(state);
            }
            catch (SchemeException error)
            {
                if (_errorHandler != null)
                {
                    List errorValue = 
                        ListFactory.create(
                            error.getCauseValue(),
                            error.getMessageValue(),
                            state.getCurrentContinuation(),
                            ScmBoolean.create(
                                error instanceof RuntimeError
                            )
                        );

                    // Avoid endless loop if the
                    // handler is buggy:
                    Function handler = _errorHandler;
                    _errorHandler = null;

                    current = handler.call(
                        state,
                        ListFactory.create(
                            errorValue
                        )
                    );
                }
                else
                {
                    throw error;
                }
            }
            ++_ticker;
        }

        return stop.getResult();
    }

    public Code compile(
        Value compilee
    ) throws SchemeException
    {
        return compile(
            getEnvironment().getStatic(),
            compilee
        );
    }

    public static Code compile(
        StaticEnvironment compilationEnv,
        Value             compilee
    ) throws SchemeException
    {
        return
            compilee
            .getCompiled(compilationEnv)
            .force();
    }

    public static Value parse(
        String expression
    ) throws SchemeException
    {
        return InputPort.create(
            new StringReader(expression)
        ).read();
    }


    public Value evaluate(
        Value evaluatee
    ) throws SchemeException
    {
        return execute(compile(evaluatee));
    }

    public Value evaluate(
        String expression
    ) throws SchemeException
    {
        return evaluate(parse(expression));
    }
}
