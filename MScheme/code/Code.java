package MScheme.code;

import MScheme.machine.Machine;
import MScheme.environment.Token;
import MScheme.environment.StaticEnvironment;
import MScheme.values.List;

import MScheme.exceptions.TypeError;
import MScheme.exceptions.CompileError;
import MScheme.exceptions.RuntimeError;


public abstract class Code
    extends Token
{
    public abstract Code executionStep(Machine machine)
        throws RuntimeError, TypeError;

    public final Code translate(
        StaticEnvironment e,
        List              arguments
    ) throws CompileError, TypeError
    {
        return new Application(
            CodeList.prepend(
                this,
                arguments.getCodeList(e)
            )
        );
    }
}
