package MScheme.syntax;

import MScheme.Value;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.util.Arity;
import MScheme.code.CompiledLambda;
import MScheme.environment.StaticEnvironment;

import MScheme.values.ListFactory;
import MScheme.values.List;
import MScheme.values.Empty;
import MScheme.values.Pair;

import MScheme.exceptions.SchemeException;


final class Lambda
            extends Syntax
{
    public final static String id
    = "$Id$";


    final static Syntax INSTANCE = new Lambda();

    private Lambda()
    {
        super(Arity.atLeast(2));
    }


    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        Value rawFormals = arguments.getHead();
        List  body       = arguments.getTail();

        final List  formals;
        final Arity arity;

        if (rawFormals.isList())
        {
            formals = rawFormals.toList();
            arity   = Arity.exactly(formals.getLength());
        }
        else
        {
            // rawFormals is an improper list.
            // This happens for lambda expressions
            // with optional parameters like
            // (lambda (x y . rest) [...])
            // or
            // (lambda args [...]).
            // The following code transforms the improper
            // or not-at-all list into a proper one and
            // counts the required parameters (two in the
            // first example and none in second).

            Value current = rawFormals;
            int  minArity = 0;
            List  result  = Empty.create();

            while (current.isPair())
            {
                Pair currentPair = current.toPair();

                ++minArity;
                result  = ListFactory.prepend(currentPair.getFirst(), result);
                current = currentPair.getSecond();
            }

            result = ListFactory.prepend(current, result);

            formals = result.getReversed();
            arity   = Arity.atLeast(minArity);
        }

        StaticEnvironment
        bodyCompilationEnv = compilationEnv.newChild(formals);

        return CompiledLambda.create(
                   arity,
                   bodyCompilationEnv,
                   body
               );
    }
}
