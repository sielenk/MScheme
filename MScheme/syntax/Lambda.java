package MScheme.syntax;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.code.CodeList;
import MScheme.code.CompiledLambda;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


final class Lambda
    extends Syntax
{
    final static Syntax INSTANCE = new Lambda();
    
    private Lambda()
    { super(Arity.atLeast(2)); }


    protected Code checkedTranslate(
        StaticEnvironment environment,
	    int               len,
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
            newEnvironment = environment.newChild(formals);

        return new CompiledLambda(
            arity,
            newEnvironment,
            body
        );
    }
}
