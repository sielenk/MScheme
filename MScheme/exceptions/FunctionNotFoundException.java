package MScheme.exceptions;

import MScheme.values.ScmString;


public class FunctionNotFoundException
    extends SchemeException
{
    public FunctionNotFoundException(String name)
    { super(ScmString.create(name)); }
}
