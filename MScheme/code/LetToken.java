package MScheme.code;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


// *** let ***

final class LetToken
    extends Token
{
    final static Token INSTANCE = new LetToken();
    
    private LetToken()
    { super(Arity.atLeast(2)); }


    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    {
        // (let ((<var> <init>) ...) <body>)

        List bindings = arguments.getHead().toList();
        List body     = arguments.getTail();

        List formals = ValueFactory.createList();
        List inits   = ValueFactory.createList();

        while (!bindings.isEmpty()) {
            List  binding = bindings.getHead().toList();

            Value formal  = binding.getHead();
            Value init    = binding.getTail().getHead();

            formals  = ValueFactory.prepend(formal, formals);
            inits    = ValueFactory.prepend(init  , inits  );

            bindings = bindings.getTail();
        }

        // ((lambda (<var> ...) <body>) <init> ...)
        return ValueFactory.prepend(
            ValueFactory.prepend(
                SyntaxFactory.getLambdaToken().getValue(),
                ValueFactory.prepend(
                    formals,
                    body
                )
            ),
            inits
        ).getCode(syntax);
    }
}

