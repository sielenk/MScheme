package MScheme.expressions.functions;

import MScheme.expressions.SExpr;
import MScheme.expressions.SSymbol;
import MScheme.expressions.SList;
import MScheme.expressions.SPair;
import MScheme.expressions.SVector;
import MScheme.expressions.SValues;

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

	    try {
		stack.push(
		    new ExpectFunctionFunc(
			((SList)pair.getCdr()).toValues()
		    )
		);

		stack.push(INSTANCE);
		return pair.getCar();
	    } catch (ClassCastException e) {
		throw new SException(pair.getCdr());
	    }
	} else if (sexpr instanceof SVector) {
	    throw new SException(sexpr);
	} else {
	    return sexpr;
	}
    }
}

