package MScheme.syntax;

import MScheme.code.Code;
import MScheme.code.CompiledAssignment;
import MScheme.environment.StaticEnvironment;
import MScheme.environment.Reference;
import MScheme.values.Value;
import MScheme.values.List;
import MScheme.values.Pair;
import MScheme.values.Symbol;

import MScheme.exceptions.*;


final class DefineToken
    extends AssignToken
{
    final static Syntax INSTANCE = new DefineToken();
    
    protected Reference getReference(
        StaticEnvironment syntax,
        Symbol            symbol
    ) throws SyntaxException
    { return syntax.define(symbol); }

    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    {
        if (arguments.getHead().isPair()) {
            //    (define (f x y) (+ x y))
            // -> (define f (lambda (x y) (+ x y)))
            Symbol symbol  = arguments.getHead().toPair().getFirst ().toSymbol();
            Value  formals = arguments.getHead().toPair().getSecond();
            Value  body    = arguments.getTail();

            return new CompiledAssignment(
                getReference(syntax, symbol),
                SyntaxFactory.getLambdaToken().translateArguments(
                    syntax,
                    Pair.create(
                        formals,
                        body
                    )
                )
            );
        } else {
            return create(syntax, arguments);
        }
    }
}
