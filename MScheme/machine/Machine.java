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

package MScheme.machine;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;

import MScheme.Code;
import MScheme.Init;
import MScheme.Value;

import MScheme.environment.Environment;
import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.SchemeException;
import MScheme.exceptions.TypeError;

import MScheme.values.InputPort;
import MScheme.values.ListFactory;
import MScheme.values.OutputPort;
import MScheme.values.ScmBoolean;
import MScheme.values.Symbol;

import MScheme.values.functions.UnaryValueFunction;
import MScheme.values.functions.ValueThunk;


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

    protected Code execute(Registers state, Value evaluationResult)
    {
        _result = evaluationResult;
        return null;
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

    private final Value _lastErrorFunc =
        new ValueThunk() {
            protected Value checkedCall()
            {
                return getLastError();
            }
        };

    private final Value _getInputFunc =
        new ValueThunk() {
            protected Value checkedCall()
            {
                return _stdin;
            }
        };

    private final Value _resetInputFunc =
        new UnaryValueFunction() {
            protected Value checkedCall(Value argument)
                throws TypeError
            {
                Value result = _stdin;
                _stdin = argument.toInputPort();
                return result;
            }
        };

    private final Value _getOutputFunc =
        new ValueThunk() {
            protected Value checkedCall()
            {
                return _stdout;
            }
        };

    private final Value _resetOutputFunc =
        new UnaryValueFunction() {
            protected Value checkedCall(Value argument)
                throws TypeError
            {
                Value result = _stdout;
                _stdout = argument.toOutputPort();
                return result;
            }
        };

    private final Environment _environment;

    private InputPort  _stdin;
    private OutputPort _stdout;

    private SchemeException _lastError      = null;
    private Value           _lastErrorValue = null;


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
                Symbol.create("last-error"),
                _lastErrorFunc
            );

            _environment.define(
                Symbol.create("current-input-port"),
                _getInputFunc
            );

            _environment.define(
                Symbol.create("reset-input-port"),
                _resetInputFunc
            );

            _environment.define(
                Symbol.create("current-output-port"),
                _getOutputFunc
            );

            _environment.define(
                Symbol.create("reset-output-port"),
                _resetOutputFunc
            );

            _environment.define(
                Symbol.create("machine-environment"),
                _environment
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

    public InputPort getInputPort()
    {
        return _stdin;
    }

    public OutputPort getOutputPort()
    {
        return _stdout;
    }

    public Value getLastError()
    {
        Value result = 
            (_lastErrorValue != null)
            ? _lastErrorValue
            : ScmBoolean.createFalse();
        
        _lastErrorValue = null;
        
        return result;
    }


    public Value execute(
        Code program
    ) throws SchemeException
    {
        Code             current = program;
        Registers        state   = new Registers(getEnvironment().getDynamic());
        StopContinuation stop    = new StopContinuation(state);

        _lastError      = null;
        _lastErrorValue = null;

        while (!stop.hasResult())
        {
            try
            {
                current = current.executionStep(state);
            }
            catch (SchemeException error)
            {
                // the Java stack is unwound
                // now go for the Scheme's ...
                
                // remember what happened, to be able
                // to "rethrow" the caught exception again
                // after the scheme stack is unwound
                _lastError      = error;
                _lastErrorValue = 
                    ListFactory.create(
                        error.getCauseValue(),
                        error.getMessageValue(),
                        state.getCurrentContinuation(),
                        ScmBoolean.create(
                            error instanceof RuntimeError
                        )
                    );

                // collect dynamic-wind thunks
                // and let the machine handle them
                current = new ContinuationFunction(
                    stop
                ).call(
                    state,
                    ListFactory.create(
                        _lastErrorValue
                    )
                );
            }
        }

        Value result = stop.getResult();
        
        if (result == _lastErrorValue)
        {
            throw _lastError;
        }
        else
        {
            return result;
        }
    }

    public Value evaluate(
        Value evaluatee
    ) throws SchemeException
    {
        return execute(compile(evaluatee));
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
            .getCode(compilationEnv)
            .force();
    }
}
