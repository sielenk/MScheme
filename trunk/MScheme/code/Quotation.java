package MScheme.code;

import MScheme.machine.Machine;
import MScheme.values.Value;
import MScheme.exceptions.SchemeException;


public class Quotation
    extends Code
{
    final private Value _quotedValue;

    public Quotation(Value quotedValue)
    { _quotedValue = quotedValue; }

    public Code executionStep(Machine machine)
        throws SchemeException
    { return machine.handleResult(_quotedValue); }
}

