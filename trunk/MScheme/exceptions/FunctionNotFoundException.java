package MScheme.exceptions;

import MScheme.values.ValueFactory;


public class FunctionNotFoundException
    extends SchemeException
{
    public FunctionNotFoundException(String name)
    { super(ValueFactory.createString(name)); }
}

