package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;

import MScheme.exceptions.SchemeException;


abstract public class TernaryFunction
    extends CheckedFunction
{
    protected TernaryFunction()
    { super(3); }
    
    abstract protected Code checkedCall(
        Machine machine,
        Value   first,
        Value   second,
        Value   third
    ) throws SchemeException;
}

