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

    public Code executionStep(Machine machine)
        throws RuntimeError, TypeError
    {
        return machine.getContinuation().invoke(
            machine,
            _literalValue
        );
    }
}
