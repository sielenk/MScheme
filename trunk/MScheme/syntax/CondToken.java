package MScheme.syntax;

import MScheme.util.Arity;
import MScheme.environment.StaticEnvironment;
import MScheme.code.Code;
import MScheme.values.List;

import MScheme.exceptions.CompileError;


final class CondToken
    extends Syntax
{
    final static Syntax INSTANCE = new CondToken();
    
    private CondToken()
    { super(Arity.atLeast(1)); }


    protected Code checkedTranslate(
        StaticEnvironment syntax,
        List              arguments
    ) throws CompileError
    {
        throw new CompileError(null);
    }
}

