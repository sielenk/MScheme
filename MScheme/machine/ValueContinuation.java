package MScheme.machine;

import MScheme.code.Code;
import MScheme.values.Value;

import MScheme.exceptions.SchemeException;


class ValueContinuation
    extends Continuation
{
    final private Value _value;

    ValueContinuation(Machine machine, Value value)
    { super(machine); _value = value; }

    protected Code internalInvoke(
        Machine machine,
        Value   value
    ) throws SchemeException
    { return machine.handleResult(_value); }
}

