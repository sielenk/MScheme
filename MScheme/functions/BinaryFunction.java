package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;

import MScheme.exceptions.SchemeException;


abstract public class BinaryFunction
    extends CheckedFunction
{
    protected BinaryFunction()
    { super(2); }

    abstract protected Code checkedCall(
        Machine machine,
        Value   first,
        Value   second
    ) throws SchemeException;
}

