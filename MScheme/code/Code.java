package MScheme.code;

import MScheme.machine.Machine;
import MScheme.environment.Token;
import MScheme.environment.StaticEnvironment;
import MScheme.values.List;
import MScheme.exceptions.SchemeException;


public abstract class Code
    extends Token
{
    public abstract Code executionStep(Machine machine)
        throws SchemeException;

    public final Code translateArguments(
        StaticEnvironment e,
        List              arguments
    ) throws SchemeException
    {
        return new CompiledApplication(
            CodeList.prepend(
                this,
                arguments.getCodeList(e)
            )
        );
    }
}
