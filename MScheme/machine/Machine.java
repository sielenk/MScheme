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

import MScheme.Value;
import MScheme.Code;

import MScheme.values.Symbol;
import MScheme.values.ListFactory;
import MScheme.values.ScmBoolean;
import MScheme.values.Function;

import MScheme.code.CodeList;
import MScheme.code.Application;

import MScheme.environment.Environment;

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
{
    public final static String id
        = "$Id$";


    private final Environment _environment;

    public Machine(Environment environment)
    {
        _environment  = environment;
    }

    public Machine()
    {
        _environment = Environment.getSchemeReportEnvironment();
    }


    public Environment getEnvironment()
    {
        return _environment;
    }


    public Value execute(
        Code programm
    )
        throws SchemeException
    {
        return execute(
            programm,
            getEnvironment()
        );
    }

    public Value evaluate(
        Value evaluatee
    )
        throws SchemeException
    {
        return evaluate(
            evaluatee,
            getEnvironment()
        );
    }


    public static Reader stdin  = new InputStreamReader (System.in );
    public static Writer stdout = new OutputStreamWriter(System.out);

    private static SchemeException   lastError      = null;
    private static Value             lastErrorValue = null;

    public static Value execute(
        Code        program,
        Environment executionEnv
    )
        throws SchemeException
    {
        Code             current = program;
        Registers        state   = new Registers(executionEnv);
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

    public static Value evaluate(
        Value       evaluatee,
        Environment executionEnv
    )
        throws SchemeException
    {
        return execute(
            evaluatee.getCode(
                executionEnv.getStatic()
            ),
            executionEnv
        );
    }


    public static Value getLastError()
    {
        Value result = 
            (lastErrorValue != null)
            ? lastErrorValue
            : ScmBoolean.createFalse();
        
        lastErrorValue = null;
        
        return result;
    }
}
