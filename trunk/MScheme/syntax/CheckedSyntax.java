package MScheme.syntax;

import java.io.Writer;
import java.io.IOException;

import MScheme.Syntax;
import MScheme.Code;

import MScheme.util.Arity;

import MScheme.values.List;

import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.SchemeException;
import MScheme.exceptions.SyntaxArityError;


abstract class CheckedSyntax
    implements Syntax
{
    public final static String id
        = "$Id";

    private final Arity _arity;

    protected CheckedSyntax(Arity arity)
    {
        _arity = arity;
    }

    protected void arityError(List arguments)
        throws SyntaxArityError
    {
        throw new SyntaxArityError(arguments, _arity);
    }

    public final Code translate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        int len = arguments.getLength();

        if (!_arity.isValid(len))
        {
            arityError(arguments);
        }

        return checkedTranslate(compilationEnv, arguments);
    }

    protected abstract Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException;
}
