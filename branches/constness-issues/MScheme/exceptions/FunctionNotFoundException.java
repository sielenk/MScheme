package MScheme.exceptions;

import MScheme.values.ScmString;


public class FunctionNotFoundException
    extends SchemeException
{
    public final static String id
        = "$Id$";

    public FunctionNotFoundException(String name)
    { super(ScmString.createConst(name)); }
}
