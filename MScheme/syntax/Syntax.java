package MScheme.syntax;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.environment.Token;
import MScheme.environment.StaticEnvironment;
import MScheme.code.Code;
import MScheme.values.List;

import MScheme.exceptions.SchemeException;
import MScheme.exceptions.ArityException;


public abstract class Syntax
    extends Token
{
    private final Arity _arity;
    
    protected Syntax(Arity arity)
    { _arity = arity; }
    
    protected abstract Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException;
    
    public final Code translateArguments(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    {
        if (!_arity.isValid(arguments.getLength())) {
            throw new ArityException(arguments, _arity);
        }

        return checkedTransform(syntax, arguments);
    }
}
