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

final class LetrecToken
    extends Syntax
{
    final static Syntax INSTANCE = new LetrecToken();
    
    private LetrecToken()
    { super(Arity.atLeast(2)); }


    protected Code checkedTranslate(
        StaticEnvironment environment,
        List              arguments
    ) throws CompileError, TypeError
    {
        // (letrec ((<var> <init>) ...) <body>)

        List bindings = arguments.getHead().toList();
        List body     = arguments.getTail();

        List formals = Empty.create();
        List inits   = Empty.create();

        // split the bindings
        while (!bindings.isEmpty()) {
            List  binding = bindings.getHead().toList();

            Value formal  = binding.getHead();
            Value init    = binding.getTail().getHead();

            formals  = Pair.create(formal, formals.toValue());
            inits    = Pair.create(init  , inits  .toValue());

            bindings = bindings.getTail();
        }

        StaticEnvironment
            newEnvironment = environment.newChild(formals);

        CodeList
            compiledBody = body.getCodeList(newEnvironment);

        // prepend the initialisations to the body
        while (!formals.isEmpty()) {
            Symbol formal = formals.getHead().toSymbol();
            Value  init   = inits  .getHead();

            compiledBody = CodeList.prepend(
                new CompiledAssignment(
                    newEnvironment.getCodeFor(formal),
                    init.getCode(newEnvironment)
                ),
                compiledBody
            );

            formals = formals.getTail();
            inits   = inits  .getTail();
        }

        return new CompiledApplication(
            CodeList.create(
                new CompiledLambda(
                    Arity.exactly(0),
                    newEnvironment,
                    compiledBody
                )
            )
        );
    }
}
