package MScheme.syntax;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.List;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.util.Arity;
import MScheme.code.*;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


// *** let ***

final class Let
            extends Syntax
{
    public final static String id
    = "$Id$";


    final static Syntax INSTANCE = new Let();

    private Let()
    {
        super(Arity.atLeast(2));
    }


    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        Symbol name;
        List   bindings;
        List   body;

        if (arguments.getHead().isSymbol())
        {
            if (arguments.getLength() < 3)
            {
                arityError(arguments);
            }
            // named let
            // (let <var> ((<var> <init>) ...) <body>)
            name     = arguments.getHead().toSymbol();
            bindings = arguments.getTail().getHead().toList();
            body     = arguments.getTail().getTail();
        }
        else
        {
            // (let ((<var> <init>) ...) <body>)
            name     = null;
            bindings = arguments.getHead().toList();
            body     = arguments.getTail();
        }

        int  count   = 0;
        List formals = Empty.create();
        List inits   = Empty.create();

        // parse the initializer list
        while (!bindings.isEmpty())
        {
            List  binding = bindings.getHead().toList();

            Value formal  = binding.getHead();
            Value init    = binding.getTail().getHead();

            ++count;
            formals  = ListFactory.prepend(formal, formals);
            inits    = ListFactory.prepend(init  , inits  );

            bindings = bindings.getTail();
        }
        // If the closure is anonymous, the order of the
        // arguments is irrelevant, as long as the inits
        // and formals match. But if it can be called by
        // the user the order has to match the definition
        // order. And a named-let-closure can be called ...
        // Since the parsing above reverses the lists,
        // they have to be reversed again here.
        formals = formals.getReversed();
        inits   = inits  .getReversed();

        if (name != null)
        {
            // for the named let, the usually anonymous
            // closure gets a name to be recursively callable.
            // to ensure this names uniqueness, it is prepended to
            // the formals list.
            formals = ListFactory.prepend(name, formals);
        }

        Code compiledProc =
            Lambda.INSTANCE.translate(
                compilationEnv,
                ListFactory.prepend(formals, body)
            );

        if (name != null)
        {
            // the "raw" closure of a named-let has one additional
            // argument, which is to be bound to the "curried"
            // closure -- the YCombinator does it's magic ...
            compiledProc = Application.create(
                               CodeList.create(
                                   YCombinator.INSTANCE.getLiteral(),
                                   compiledProc
                               )
                           );
        }

        return compiledProc.translate(compilationEnv, inits);
    }
}
