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

final class Let
    extends Syntax
{
    final static Syntax INSTANCE = new Let();
    
    private Let()
    { super(Arity.atLeast(2)); }


    protected Code checkedTranslate(
        StaticEnvironment environment,
	    int               len,
        List              arguments
    ) throws CompileError, TypeError
    {
        Symbol name;
        List   bindings;
        List   body;

        if (arguments.getHead().isSymbol()) {
	        if (len < 3) {
		        arityError(arguments);
		    }
	        // named let
            // (let <var> ((<var> <init>) ...) <body>)
		    name     = arguments.getHead().toSymbol();
            bindings = arguments.getTail().getHead().toList();
            body     = arguments.getTail().getTail();
	    } else {
            // (let ((<var> <init>) ...) <body>)
            name     = null;
            bindings = arguments.getHead().toList();
            body     = arguments.getTail();
	    }

        int  count   = 0;
        List formals = Empty.create();
        List inits   = Empty.create();

        // parse the initializer list
        while (!bindings.isEmpty()) {
            List  binding = bindings.getHead().toList();

            Value formal  = binding.getHead();
            Value init    = binding.getTail().getHead();

            ++count;
            formals  = ValueFactory.prepend(formal, formals);
            inits    = ValueFactory.prepend(init  , inits  );

            bindings = bindings.getTail();
        }

        if (name != null) {
	        // for the named let, the usually anonymous
		    // closure gets a name to be recursively callable.
		    // to ensure this names uniqueness, it is added to
		    // the formals list.
	        formals = ValueFactory.prepend(name, formals);

            // if the closure is anonymous, the order of the
	        // arguments is irrelevant, if the inits and formals
		    // match. But if it can be called by the user the
		    // order has to match the definition order.
		    // And since the parsing above reverses the lists,
		    // they have to be reversed again here.
		    formals = formals.getReversed();
		    inits   = inits  .getReversed();
	    }

        StaticEnvironment innerEnvironment =
	        environment.newChild(formals);

        CompiledLambda compiledProc =
	        new CompiledLambda(
                Arity.exactly(count),
                innerEnvironment,
                body
            );

        if (name != null) {
	        compiledProc.setSelf(
		        innerEnvironment.getCodeFor(name)
			);
	    }

        return Application.create(
            CodeList.prepend(
		        compiledProc,
		        inits.getCodeList(environment)
            )
        );
    }
}
