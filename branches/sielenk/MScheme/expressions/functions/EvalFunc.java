package MScheme.expressions.functions;

import MScheme.expressions.SExpr;
import MScheme.expressions.SSymbol;
import MScheme.expressions.SPair;
import MScheme.expressions.SVector;

import MScheme.exceptions.SException;
import MScheme.machine.ContinuationStack;
import MScheme.environment.Environment;

public class EvalFunc extends Function
{
    public final static Function INSTANCE = new EvalFunc();
    
    private EvalFunc() { }
    

    public SExpr call(
	ContinuationStack stack,
	Environment environment,
	SExpr sexpr
    ) throws SException {
	if (sexpr instanceof SSymbol) {
	    return environment.lookup((SSymbol)sexpr);
	} else if (sexpr instanceof SPair) {
	    SPair pair = (SPair)sexpr;
		
            stack.push(
		new ExpectFunctionFunc(
		    pair.getCdr()
		)
	    );

            stack.push(INSTANCE);
	    return pair.getCar();

	} else if (sexpr instanceof SVector) {
	    throw new SException(sexpr);
	} else {
	    return sexpr;
	}
    }
}

