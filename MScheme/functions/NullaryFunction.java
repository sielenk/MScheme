package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;

import MScheme.exceptions.SchemeException;


abstract public class NullaryFunction
    extends CheckedFunction
{
    protected NullaryFunction()
    { super(0); }
    
    abstract protected Code checkedCall(
        Machine machine
    ) throws SchemeException;
}

