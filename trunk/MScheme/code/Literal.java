package MScheme.code;

import MScheme.Value;
import MScheme.machine.Result;
import MScheme.machine.Registers;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public final class Literal
    extends Result
{
    private final Value _value;

    private Literal(Value value)
    { _value = value; }

    public static Literal create(Value value)
    { return new Literal(value); }


    protected Value getValue(Registers registers)
    { return _value; }
}
