package MScheme.machine;

import MScheme.Value;
import MScheme.Code;

import MScheme.environment.Environment;

import MScheme.exceptions.*;


final class AbortContinuation
    extends Continuation
{
    public final static String id
        = "$Id$";


    private Value _result;

    AbortContinuation(Registers state)
    { super(state); _result = null; }

    Value getResult()
    { return _result; }

    boolean hasResult()
    { return _result != null; }

    protected Code execute(Registers state, Value evaluationResult)
    { _result = evaluationResult; return null; }


    protected String debugString()
    { return "abort"; }
}


public final class Machine
{
    public final static String id
        = "$Id$";


    private final Environment _environment;
    
    public Machine(Environment environment)
    { _environment  = environment; }

    public Machine()
    { this(Environment.getSchemeReportEnvironment()); }


    public Environment getEnvironment()
    { return _environment; }


    public Value execute(Code program)
        throws SchemeException
    {
        Code              next  = program;
        Registers         state = new Registers(getEnvironment());
        AbortContinuation abort = new AbortContinuation(state);

        try {
            while (!abort.hasResult()) {
                next = next.executionStep(state);
            }
        }
        catch (SchemeException e) {
            System.err.print(
                state.getContinuation().toString()
            );
            
            throw e;
        }

        return abort.getResult();
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
