package MScheme.code;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


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
