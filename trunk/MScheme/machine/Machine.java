package MScheme.machine;

import MScheme.values.*;
import MScheme.code.*;
import MScheme.exceptions.*;
import MScheme.functions.UnaryFunction;
import MScheme.environment.*;


class AbortContinuation
    extends Continuation
{
    private Value _result;

    AbortContinuation(Machine machine)
    {
        super(machine);
        _result = null;
    }

    Value getResult()
    { return _result; }

    protected Code execute(Machine machine, Value evaluationResult)
    {
        _result = evaluationResult;
        return null;
    }
}


public class Machine
{
    private DynamicEnvironment _environment;
    private Continuation       _continuation;
    
    public Machine(DynamicEnvironment environment)
    {
        _environment  = environment;
        _continuation = null;
    }

    public Machine()
    { this(DynamicEnvironment.getSchemeReportEnvironment()); }


    public DynamicEnvironment getEnvironment()
    { return _environment; }

    public void setEnvironment(DynamicEnvironment newEnvironment)
    { _environment = newEnvironment; }
    

    public UnaryFunction getCurrentContinuation()
    { return _continuation.getFunction(); }

    void setContinuation(Continuation newContinuation)
    { _continuation = newContinuation; }
    
    Continuation getContinuation()
    { return _continuation; }

    
    public Value execute(Code program)
        throws RuntimeError, TypeError
    {
        Code nextInstruction = program;
        
        setContinuation(null);
        AbortContinuation abort = new AbortContinuation(this);
        
        while (nextInstruction != null) {
            nextInstruction = nextInstruction.executionStep(this);
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
