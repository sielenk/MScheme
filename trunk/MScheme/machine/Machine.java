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

import java.io.Reader;
import java.io.Writer;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.StringReader;

import MScheme.Init;

import MScheme.Value;
import MScheme.Code;

import MScheme.values.Symbol;
import MScheme.values.ListFactory;
import MScheme.values.ScmBoolean;
import MScheme.values.Function;
import MScheme.values.InputPort;
import MScheme.values.OutputPort;

import MScheme.values.functions.ValueThunk;
import MScheme.values.functions.UnaryValueFunction;

import MScheme.code.Application;

import MScheme.environment.Environment;
import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.*;


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


    private SchemeException   lastError      = null;
    private Value             lastErrorValue = null;

    public Value execute(
        Code program
    ) throws SchemeException
    {
        Code             current = program;
        Registers        state   = new Registers(getEnvironment());
        StopContinuation stop    = new StopContinuation(state);

        lastError      = null;
        lastErrorValue = null;

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
                lastError      = error;
                lastErrorValue = 
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
                        lastErrorValue
                    )
                );
            }
        }

        Value result = stop.getResult();
        
        if (result == lastErrorValue)
        {
            throw lastError;
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
        StaticEnvironment global = getEnvironment().getStatic();
        
        return execute(
            evaluatee.getCode(
                global 
            ).force(
                global
            )
        );
    }


    public Value getLastError()
    {
        Value result = 
            (lastErrorValue != null)
            ? lastErrorValue
            : ScmBoolean.createFalse();
        
        lastErrorValue = null;
        
        return result;
    }
}
