package MScheme.code;

import MScheme.machine.State;
import MScheme.environment.Token;
import MScheme.environment.StaticEnvironment;
import MScheme.values.List;

import MScheme.exceptions.TypeError;
import MScheme.exceptions.CompileError;
import MScheme.exceptions.RuntimeError;


public abstract class Code
    extends Token
{
    public abstract Code executionStep(State state)
        throws RuntimeError, TypeError;

    public final Code translate(
        StaticEnvironment e,
        List              arguments
    ) throws CompileError, TypeError
    {
        return Application.create(
            CodeList.prepend(
                this,
                arguments.getCodeList(e)
            )
        );
    }
}
