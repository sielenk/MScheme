package MScheme.syntax;

import MScheme.util.Arity;

import MScheme.Value;
import MScheme.List;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.environment.StaticEnvironment;
import MScheme.environment.Reference;

import MScheme.values.ListFactory;
import MScheme.values.Pair;
import MScheme.values.Symbol;

import MScheme.exceptions.*;


final class Define
    extends Syntax
{
    public final static String id
        = "$Id$";


    final static Syntax INSTANCE = new Define();

    private Define()
    { super(Arity.atLeast(2)); }

    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws CompileError, TypeError
    {
        if (arguments.getHead().isPair()) {
            //    (define (f x y) (+ x y))
            // -> (define f (lambda (x y) (+ x y)))
            Symbol symbol  = arguments.getHead().toPair().getFirst ().toSymbol();
            Value  formals = arguments.getHead().toPair().getSecond();
            List   body    = arguments.getTail();

            return Set.translate(
                compilationEnv.define(symbol),
                Lambda.INSTANCE.translate(
                    compilationEnv,
                    ListFactory.prepend(
                        formals,
                        body
                    )
                )
            );
        } else {
            compilationEnv.define(arguments.getHead().toSymbol());
            
            // call the translate instead of checkedTranslate 
            // to let Set check the argument count again
            return Set.INSTANCE.translate(
                compilationEnv,
                arguments
            );
        }
    }
}
