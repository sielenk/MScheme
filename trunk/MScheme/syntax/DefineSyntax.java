package MScheme.syntax;

import MScheme.util.Arity;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.code.CodeList;
import MScheme.code.Application;
import MScheme.environment.Environment;
import MScheme.environment.StaticEnvironment;
import MScheme.environment.Reference;
import MScheme.values.ValueFactory;
import MScheme.Value;
import MScheme.values.List;
import MScheme.values.Empty;
import MScheme.values.Pair;
import MScheme.values.Symbol;
import MScheme.values.InputPort;
import MScheme.functions.ApplyFunction;

import MScheme.exceptions.*;


final class Macro
    extends Syntax
{
    private final static Code
        _apply = ApplyFunction.INSTANCE.getLiteral();

    private final Code              _transformer;
    private final StaticEnvironment _def_env;
    
    Macro(Code transformer, StaticEnvironment def_env)
    {
        super(Arity.atLeast(0));
        _transformer = transformer;
	    _def_env     = def_env;
	}
    
    protected Code checkedTranslate(
        StaticEnvironment use_env,
	    int               len,
        List              arguments
    ) throws CompileError, TypeError
    {
        try {
	        // (apply tranformer def_env use_env args)
	
            return new Machine(
	            Environment.getImplementationEnvironment()
	        ).execute(
	            Application.create(
		            CodeList.prepend(
			            _apply,
    		            CodeList.prepend(
    			            _transformer,
        		            CodeList.create(
        					    _def_env.getLiteral(),
          					     use_env.getLiteral(),
           				        arguments.toValue().getLiteral()
						    )
   					    )
			        )
		        )
    	    ).getCode(use_env);
	    }
	    catch (RuntimeError e) {
	        throw new CompileError(e.getCause());
	    }
    }
}

final class DefineSyntax
    extends Syntax
{
    final static Syntax INSTANCE = new DefineSyntax();

    private DefineSyntax()
    { super(Arity.exactly(2)); }

    protected Code checkedTranslate(
        StaticEnvironment syntax,
	    int               len,
        List              arguments
    ) throws CompileError, TypeError
    {
        Symbol symbol = arguments.getHead().toSymbol();
        Value  value  = arguments.getTail().getHead();

        try {
            Macro macro = new Macro(
                new Machine(Environment.getImplementationEnvironment())
                    .evaluate(value)
                    .toFunction()
				    .getLiteral(),
				syntax
	        );

            syntax.defineSyntax(symbol, macro);
	        Environment
		        .getImplementationEnvironment()
			    .getStatic()
			    .defineSyntax(symbol, macro);
	    }
	    catch (RuntimeError e) {
	        throw new CompileError(e.getCause());
	    }

        return Empty.create().getLiteral();
    }
}
