package MScheme;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.environment.StaticEnvironment;
import MScheme.values.List;

import MScheme.exceptions.TypeError;
import MScheme.exceptions.CompileError;
import MScheme.exceptions.SyntaxArityError;


public abstract class Syntax
    implements Translator
{
    public final static String id
        = "$Id$";

    private final Arity _arity;
    
    protected Syntax(Arity arity)
    { _arity = arity; }

    protected void arityError(List arguments)
        throws SyntaxArityError
    { throw new SyntaxArityError(arguments, _arity); }

    protected abstract Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws CompileError, TypeError;

    public final Code translate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws CompileError, TypeError
    {
        int len = arguments.getLength();

        if (!_arity.isValid(len)) {
            arityError(arguments);
        }

        return checkedTranslate(compilationEnv, arguments);
    }
}
