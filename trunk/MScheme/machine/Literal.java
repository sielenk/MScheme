package MScheme.machine;

import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.exceptions.SchemeException;


public class Literal
    extends Code
{
    final private Value _literalValue;

    public Literal(Value literalValue)
    { _literalValue = literalValue; }

    public Code executionStep(Machine machine)
        throws SchemeException
    {
        return machine.getContinuation().invoke(
            machine,
            _literalValue
        );
    }
}
