package MScheme.syntax;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.code.CodeList;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


// *** let ***

final class LetToken
    extends Syntax
{
    final static Syntax INSTANCE = new LetToken();
    
    private LetToken()
    { super(Arity.atLeast(2)); }


    protected Code checkedTranslate(
        StaticEnvironment syntax,
        List              arguments
    ) throws CompileError, TypeError
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
        return LambdaToken.INSTANCE
            .translate(
                syntax,
                ValueFactory.prepend(
                    formals,
                    body
                )
            )
            .translate(
                syntax,
                inits
            );
    }
}
