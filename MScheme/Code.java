package MScheme;

import MScheme.code.CodeList;
import MScheme.code.Application;

import MScheme.machine.Registers;
import MScheme.environment.Token;
import MScheme.environment.StaticEnvironment;
import MScheme.values.List;

import MScheme.exceptions.TypeError;
import MScheme.exceptions.CompileError;
import MScheme.exceptions.RuntimeError;


public abstract class Code
    extends Token
{
    public abstract Code executionStep(Registers registers)
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
