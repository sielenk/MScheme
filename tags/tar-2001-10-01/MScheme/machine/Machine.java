package MScheme.machine;

import MScheme.Value;
import MScheme.Code;

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


    public Value execute(Code program)
    throws SchemeException
    {
        return execute(program, null);
    }

    public Value execute(Code program, Function errorFunction)
    throws SchemeException
    {
        Code              next  = program;
        Registers         state = new Registers(getEnvironment());
        AbortContinuation abort = new AbortContinuation(state);

        while (!abort.hasResult())
        {
            try
            {
                next = next.executionStep(state);
            }
            catch (SchemeException error)
            {
                if (errorFunction != null)
                {
                    next = errorFunction.call(
                               state,
                               ListFactory.create(
                                   ScmBoolean.createFalse(),
                                   error.getCauseValue(),
                                   error.getMessageValue()
                               )
                           );
                }
                else
                {
                    throw error;
                }
            }
        }

        return abort.getResult();
    }

    public Value evaluate(Value evaluatee)
    throws SchemeException
    {
        return evaluate(evaluatee, null);
    }

    public Value evaluate(Value evaluatee, Function errorFunction)
    throws SchemeException
    {
        Code program;

        try
        {
            program = evaluatee.getCode(
                          getEnvironment().getStatic()
                      );
        }
        catch (SchemeException error)
        {
            if (errorFunction != null)
            {
                program = Application.create(
                              CodeList.create(
                                  errorFunction.getLiteral(),
                                  ScmBoolean.createTrue().getLiteral(),
                                  error.getCauseValue().getLiteral(),
                                  error.getMessageValue().getLiteral()
                              )
                          );
            }
            else
            {
                throw error;
            }
        }

        return execute(
                   program,
                   errorFunction
               );
    }
}
