package MScheme.machine;

import MScheme.values.*;
import MScheme.code.*;
import MScheme.exceptions.*;
import MScheme.functions.UnaryFunction;
import MScheme.environment.*;


public class Machine
{
    private Environment  _environment;
    
    public Machine(Environment environment)
    { _environment  = environment; }

    public Machine()
    { this(Environment.getSchemeReportEnvironment()); }


    public Environment getEnvironment()
    { return _environment; }

    public void setEnvironment(Environment newEnvironment)
    { _environment = newEnvironment; }
    

    public Value execute(Code program)
        throws RuntimeError, TypeError
    {
        Code  nextInstruction = program;
        State state           = new State(getEnvironment());
        
        while (!state.isHalt()) {
            nextInstruction = nextInstruction.executionStep(state);
        }

        return state.getResult();
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
