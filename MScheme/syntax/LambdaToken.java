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


final class LambdaToken
    extends Syntax
{
    final static Syntax INSTANCE = new LambdaToken();
    
    private LambdaToken()
    { super(Arity.atLeast(2)); }


    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
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
            
            lastPair.setSecond(
                ValueFactory.createList(
                    lastPair.getSecond().toSymbol()
                )
            );
            
            formals = head.getSecond().toList();
            arity   = Arity.atLeast(minArity);
        }

        StaticEnvironment
            compiledFormals = syntax.newChild(formals);

        Code compiledBody =
            BeginToken.INSTANCE.translateArguments(
                compiledFormals,
                body
            );
                    
        return new CompiledLambda(
            arity,
            compiledFormals,
            compiledBody
        );
    }
}

