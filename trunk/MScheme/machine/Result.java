package MScheme.machine;

import MScheme.Value;
import MScheme.Code;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public abstract class Result
    extends Code
{
    public final static String id
        = "$Id$";

    protected abstract Value getValue(Registers registers)
        throws RuntimeError;

    public final Code executionStep(Registers registers)
        throws RuntimeError, TypeError
    {
        return registers.getContinuation().invoke(
            registers,
            getValue(registers)
        );
    }
}
