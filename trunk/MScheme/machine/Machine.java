package MScheme.machine;

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


final class AbortContinuation
            extends Continuation
{
    public final static String id
    = "$Id$";


    private Value _result;

    AbortContinuation(Registers state)
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


    private final static Symbol errorTag
        = Symbol.create("error-tag");

    public Value execute(Code program)
        throws SchemeException
    {
        Code              next  = program;
        Registers         state = new Registers(getEnvironment());
        AbortContinuation abort = new AbortContinuation(state);

        SchemeException   lastError      = null;
        Value             lastErrorValue = null;

        while (!abort.hasResult())
        {
            try
            {
                next = next.executionStep(state);
            }
            catch (SchemeException error)
            {
                lastError      = error;
                lastErrorValue = 
                    ListFactory.create(
                        errorTag,
                        error.getCauseValue(),
                        error.getMessageValue()
                    );

                next = new ContinuationFunction(
                    abort
                ).call(
                    state,
                    ListFactory.create(
                        lastErrorValue
                    )
                );
            }
        }

        Value result = abort.getResult();
        
        if (result == lastErrorValue) 
        {
            throw lastError;
        }
        else
        {
            return result;
        }
    }

    public Value evaluate(Value evaluatee)
        throws SchemeException
    {
        return execute(
            evaluatee.getCode(
                getEnvironment().getStatic()
            )
        );
    }
}
