package MScheme.expressions.functions;

import MScheme.expressions.SExpr;
import MScheme.expressions.SSymbol;
import MScheme.expressions.SPair;
import MScheme.expressions.SVector;

import MScheme.exceptions.SExpectedFunctionException;
import MScheme.machine.ContinuationStack;
import MScheme.environment.Environment;

public class ExpectFunctionFunc extends Function
{
    private SExpr _arguments;
    
    public ExpectFunctionFunc(
	SExpr arguments
    ) {
	_arguments = arguments;
    }
    
    public SExpr call(
	ContinuationStack stack,
	Environment       environment,
	SExpr             sexpr
    ) throws SExpectedFunctionException {
	if (sexpr instanceof Function) {
	    stack.push((Function)sexpr);
	    return _arguments;
	} else {
	    throw new SExpectedFunctionException(sexpr);
	}
    }
}
