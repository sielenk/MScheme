package MScheme.syntax;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.*;
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
        StaticEnvironment environment,
        List              arguments
    ) throws CompileError, TypeError
    {
        // (let ((<var> <init>) ...) <body>)

        List bindings = arguments.getHead().toList();
        List body     = arguments.getTail();

        int  count   = 0;
        List formals = Empty.create();
        List inits   = Empty.create();

        while (!bindings.isEmpty()) {
            List  binding = bindings.getHead().toList();

            Value formal  = binding.getHead();
            Value init    = binding.getTail().getHead();

            ++count;
            formals  = ValueFactory.prepend(formal, formals);
            inits    = ValueFactory.prepend(init  , inits  );

            bindings = bindings.getTail();
        }

        StaticEnvironment
            newEnvironment = environment.newChild(formals);

        return new CompiledApplication(
            CodeList.prepend(
                new CompiledLambda(
                    Arity.exactly(count),
                    newEnvironment,
                    body
                ),
                inits.getCodeList(environment)
            )
        );
    }
}
