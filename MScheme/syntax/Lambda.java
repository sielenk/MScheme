package MScheme.syntax;

import MScheme.Value;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.util.Arity;
import MScheme.code.CompiledLambda;
import MScheme.environment.StaticEnvironment;
import MScheme.values.List;
import MScheme.values.Pair;
import MScheme.values.ValueFactory;

import MScheme.exceptions.*;


final class Lambda
    extends Syntax
{
    public final static String id
        = "$Id$";

    final static Syntax INSTANCE = new Lambda();
    
    private Lambda()
    { super(Arity.atLeast(2)); }


    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws CompileError, TypeError
    {
        Value rawFormals = arguments.getHead();
        List  body       = arguments.getTail();

        final List  formals;
        final Arity arity;
        
        if (rawFormals.isList()) {
            formals = rawFormals.toList();
            arity   = Arity.exactly(formals.getLength());
        } else {
            Pair head     = Pair.create(null, rawFormals);
            Pair lastPair = head;
            int  minArity = 0;
        
            while (lastPair.getSecond().isPair()) {
                lastPair = lastPair.getSecond().toPair();
                minArity++;
            }
            
            try {
                lastPair.setSecond(
                    ValueFactory.createList(
                        lastPair.getSecond().toSymbol()
                    )
                );
            }
            catch (ImmutableException e) {
                throw new RuntimeException(
                    "unexpected ImmutableException"
                );
            }
            
            formals = head.getSecond().toList();
            arity   = Arity.atLeast(minArity);
        }

        StaticEnvironment
            bodyCompilationEnv = compilationEnv.newChild(formals);

        return new CompiledLambda(
            arity,
            bodyCompilationEnv,
            body
        );
    }
}
