package MScheme.code;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


final class CondToken
    extends Token
{
    final static Token INSTANCE = new CondToken();
    
    private CondToken()
    { super(Arity.atLeast(1)); }


    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    {
        throw new UnimplementedException(getValue());
    }
}

