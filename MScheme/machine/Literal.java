package MScheme.machine;

import MScheme.code.Code;
import MScheme.values.Value;

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
