package MScheme.code;

import MScheme.machine.Machine;
import MScheme.environment.StaticEnvironment;
import MScheme.values.List;
import MScheme.exceptions.SchemeException;
import MScheme.exceptions.SyntaxException;


public abstract class Syntax
    extends Code
{
    public final Code executionStep(Machine machine)
        throws SyntaxException
    { throw new SyntaxException(null); }
    
    public abstract Code translateArguments(
        StaticEnvironment e,
        List              arguments
    ) throws SchemeException;
}

