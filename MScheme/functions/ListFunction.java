package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.SchemeException;


public class ListFunction
    extends Function
{
    public final static ListFunction INSTANCE = new ListFunction();


    public Code call(
        Machine machine,
        List    arguments
    ) throws SchemeException
    { return machine.handleResult(arguments); }
}

