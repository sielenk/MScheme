package MScheme.syntax;

import MScheme.Value;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.code.Assignment;
import MScheme.environment.StaticEnvironment;
import MScheme.environment.Reference;
import MScheme.values.ValueFactory;
import MScheme.values.List;
import MScheme.values.Pair;
import MScheme.values.Symbol;

import MScheme.exceptions.*;


final class Define
    extends Assign
{
    final static Syntax INSTANCE = new Define();
    
    protected Reference getReference(
        StaticEnvironment syntax,
        Symbol            symbol
    ) throws CompileError
    { return syntax.define(symbol); }

    protected Code checkedTranslate(
        StaticEnvironment syntax,
	    int               len,
        List              arguments
    ) throws CompileError, TypeError
    {
        if (arguments.getHead().isPair()) {
            //    (define (f x y) (+ x y))
            // -> (define f (lambda (x y) (+ x y)))
            Symbol symbol  = arguments.getHead().toPair().getFirst ().toSymbol();
            Value  formals = arguments.getHead().toPair().getSecond();
            List   body    = arguments.getTail();

            return new Assignment(
                getReference(syntax, symbol),
                Lambda.INSTANCE.translate(
                    syntax,
                    ValueFactory.prepend(
                        formals,
                        body
                    )
                )
            );
        } else {
            return super.checkedTranslate(syntax, len, arguments);
        }
    }
}
