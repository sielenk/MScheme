package MScheme.syntax;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.util.Arity;
import MScheme.code.CodeList;
import MScheme.code.CompiledLambda;
import MScheme.code.Application;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


// *** letrec ***

final class Letrec
            extends Syntax
{
    public final static String id
    = "$Id$";


    final static Syntax INSTANCE = new Letrec();

    private Letrec()
    {
        super(Arity.atLeast(2));
    }


    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        // (letrec ((<var> <init>) ...) <body>)

        List bindings = arguments.getHead().toList();
        List body     = arguments.getTail();

        List formals = Empty.create();
        List inits   = Empty.create();

        // split the bindings
        while (!bindings.isEmpty())
        {
            List  binding = bindings.getHead().toList();

            Value formal  = binding.getHead();
            Value init    = binding.getTail().getHead();

            formals  = ListFactory.prepend(formal, formals);
            inits    = ListFactory.prepend(init  , inits  );

            bindings = bindings.getTail();
        }

        StaticEnvironment
        bodyCompilationEnv = compilationEnv.newChild(formals);

        CodeList
        compiledBody = body.getCodeList(bodyCompilationEnv);

        // prepend the initialisations to the body
        while (!formals.isEmpty())
        {
            Symbol formal = formals.getHead().toSymbol();
            Value  init   = inits  .getHead();

            compiledBody = CodeList.prepend(
                               Set.translate(
                                   formal.getReference(bodyCompilationEnv),
                                   init  .getCode     (bodyCompilationEnv)
                               ),
                               compiledBody
                           );

            formals = formals.getTail();
            inits   = inits  .getTail();
        }

        return Application.create(
                   CodeList.create(
                       CompiledLambda.create(
                           Arity.exactly(0),
                           bodyCompilationEnv,
                           compiledBody
                       )
                   )
               );
    }
}
