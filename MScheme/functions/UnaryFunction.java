package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;

import MScheme.exceptions.SchemeException;


abstract public class UnaryFunction
    extends CheckedFunction
{
    protected UnaryFunction()
    { super(1); }

    abstract protected Code checkedCall(
        Machine machine,
        Value   first
    ) throws SchemeException;
}

