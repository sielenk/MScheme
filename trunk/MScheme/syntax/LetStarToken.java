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

final class LetStarToken
    extends Syntax
{
    final static Syntax INSTANCE = new LetStarToken();
    
    private LetStarToken()
    { super(Arity.atLeast(2)); }


    protected Code checkedTranslate(
        StaticEnvironment environment,
        List              arguments
    ) throws CompileError, TypeError
    {
        // (let* ((<var> <init>) ...) <body>)

        List bindings = arguments.getHead().toList();
        List body     = arguments.getTail();
        int  count    = bindings.getLength();

        if (count == 0) {
            return CompiledSequence.create(
                body.getCodeList(environment)
            );
        }

        StaticEnvironment[] environments  = new StaticEnvironment[count + 1];
        Code             [] compiledInits = new Code[count];

        {
            environments[0] = environment;

            for (int i = 0; i < count; ++i) {
                List  binding = bindings.getHead().toList();

                Symbol formal  = binding.getHead().toSymbol();
                Value  init    = binding.getTail().getHead();

                compiledInits[i    ] = init.getCode(environments[i]);
                environments [i + 1] = environments[i].newChild(formal);

                bindings = bindings.getTail();
            }
        }

        Code lambda = new CompiledLambda(
            Arity.exactly(1),
            environments[count],
            body
        );

        int i = count - 1;
        while (true) {
            Code application = new CompiledApplication(
                CodeList.create(
                    lambda,
                    compiledInits[i]
                )
            );

            if (i == 0) {
                return application;
            }

            lambda = new CompiledLambda(
                Arity.exactly(1),
                environments[i],
                application
            );

            --i;
        }
    }
}
