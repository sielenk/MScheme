package MScheme.syntax;

import MScheme.util.Arity;
import MScheme.environment.StaticEnvironment;
import MScheme.code.Code;
import MScheme.values.List;

import MScheme.exceptions.SchemeException;
import MScheme.exceptions.UnimplementedException;


final class CondToken
    extends Syntax
{
    final static Syntax INSTANCE = new CondToken();
    
    private CondToken()
    { super(Arity.atLeast(1)); }


    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    {
        throw new UnimplementedException(null);
    }
}

