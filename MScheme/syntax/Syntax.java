package MScheme.syntax;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.environment.Token;
import MScheme.environment.StaticEnvironment;
import MScheme.code.Code;
import MScheme.values.List;

import MScheme.exceptions.TypeError;
import MScheme.exceptions.CompileError;
import MScheme.exceptions.SyntaxArityError;


public abstract class Syntax
    extends Token
{
    private final Arity _arity;
    
    protected Syntax(Arity arity)
    { _arity = arity; }
    
    protected abstract Code checkedTranslate(
        StaticEnvironment syntax,
        List              arguments
    ) throws CompileError, TypeError;
    
    public final Code translate(
        StaticEnvironment syntax,
        List              arguments
    ) throws CompileError, TypeError
    {
        if (!_arity.isValid(arguments.safeGetLength())) {
            throw new SyntaxArityError(arguments, _arity);
        }

        return checkedTranslate(syntax, arguments);
    }
}
