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


    private final static Symbol errorTag
        = Symbol.create("error-tag");

    public static Value execute(
        Code        program,
        Environment executionEnv
    )
        throws SchemeException
    {
        Code              next  = program;
        Registers         state = new Registers(executionEnv);
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
                // the java stack is unwound
                // now go for the scheme's ...

                // remember what happened, to be able
                // to "rethrow" the caught exception again
                // after the scheme stack is unwound
                lastError      = error;
                lastErrorValue = 
                    ListFactory.create(
                        errorTag,
                        error.getCauseValue(),
                        error.getMessageValue()
                    );

                // collect dynamic-wind thunks
                // and let the machine handle them
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
}
