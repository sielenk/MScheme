package MScheme.machine;

import MScheme.Value;
import MScheme.Code;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public class Literal
    extends Code
{
    final private Value _literalValue;

    public Literal(Value literalValue)
    { _literalValue = literalValue; }

    public Code executionStep(Registers registers)
        throws RuntimeError, TypeError
    {
        return registers.getContinuation().invoke(
            registers,
            _literalValue
        );
    }
}
