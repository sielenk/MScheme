package MScheme.syntax;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.List;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.*;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


// *** let* ***

final class LetStar
    extends Syntax
{
    public final static String id
        = "$Id$";


    final static Syntax INSTANCE = new LetStar();
    
    private LetStar()
    { super(Arity.atLeast(2)); }


    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        // (let* ((<var> <init>) ...) <body>)

        List bindings = arguments.getHead().toList();
        List body     = arguments.getTail();

        if (bindings.isEmpty()) {
            // special handling because the helper won't
            // create a new environment in this case

            return Application.create(
                CodeList.create(
                    new CompiledLambda(
                        Arity.exactly(0),
                        compilationEnv.newChild(),
                        body
                    )
                )
            );
        } else {
            return
                new LetStarHelper(body)
                .translate(compilationEnv, bindings);
        }
    }
}

final class LetStarHelper
{
    public final static String id
        = "$Id$";


    private final List _body;

    LetStarHelper(List body)
    { _body = body; }

    Code translate(
        StaticEnvironment outerEnvironment,
        List              bindings
    ) throws SchemeException
    {
        if (bindings.isEmpty()) {
            return Sequence.create(
                _body.getCodeList(outerEnvironment)
            );
        } else {
            List binding = bindings.getHead().toList();

            Symbol formal  = binding.getHead().toSymbol();
            Value  init    = binding.getTail().getHead();

            StaticEnvironment
                innerEnvironment = outerEnvironment.newChild(formal);

            Code innerBody = translate(
                innerEnvironment,
                bindings.getTail()
            );

            Code lambda = new CompiledLambda(
                Arity.exactly(1),
                innerEnvironment,
                innerBody
            );

            return Application.create(
                CodeList.create(
                    lambda,
                    init.getCode(outerEnvironment)
                )
            );
        }
    }
}
