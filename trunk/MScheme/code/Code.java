package MScheme.code;

import MScheme.machine.Machine;
import MScheme.environment.StaticEnvironment;
import MScheme.values.List;
import MScheme.exceptions.SchemeException;


public abstract class Code
    extends Token
{
    public abstract Code executionStep(Machine machine)
        throws SchemeException;

    public Code translateArguments(
        StaticEnvironment e,
        List              arguments
    ) throws SchemeException
    { return CodeList.prepend(this, arguments.getCodeList(e)); }
}
