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

    AbortContinuation(Registers registers)
    { super(registers); _result = null; }

    Value getResult()
    { return _result; }

    boolean hasResult()
    { return _result != null; }

    protected Code execute(Registers registers, Value evaluationResult)
    { _result = evaluationResult; return null; }
}


final public class Machine
{
    public final static String id
        = "$Id$";

    final private Environment _environment;
    
    public Machine(Environment environment)
    { _environment  = environment; }

    public Machine()
    { this(Environment.getSchemeReportEnvironment()); }


    public Environment getEnvironment()
    { return _environment; }


    public Value execute(Code program)
        throws RuntimeError, TypeError
    {
        Code              nextInstruction = program;
        Registers         registers       = new Registers(getEnvironment());
        AbortContinuation abort           = new AbortContinuation(registers);

        while (!abort.hasResult()) {
            nextInstruction = nextInstruction.executionStep(registers);
        }

        return abort.getResult();
    }

    public Value evaluate(Value evaluatee)
        throws RuntimeError, CompileError, TypeError
    {
        return execute(
            evaluatee.getCode(
                getEnvironment().getStatic()
            )
        );
    }
}
