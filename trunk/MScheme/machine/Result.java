package MScheme.machine;

import MScheme.Value;
import MScheme.Code;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public abstract class Result
    extends Code
{
    protected abstract Value getValue(Registers registers);

    public final Code executionStep(Registers registers)
        throws RuntimeError, TypeError
    {
        return registers.getContinuation().invoke(
            registers,
            getValue(registers)
        );
    }
}
