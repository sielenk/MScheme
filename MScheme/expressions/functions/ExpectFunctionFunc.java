package MScheme.expressions.functions;


import MScheme.expressions.SExpr;
import MScheme.expressions.SFunction;

import MScheme.exceptions.SExpectedFunctionException;

import MScheme.util.Values;
import MScheme.machine.Machine;

import MScheme.environment.Environment;


public class ExpectFunctionFunc extends Function
{
    private Values _arguments;

    public ExpectFunctionFunc(
        Values arguments
    ) {
        super(1, 1);
        _arguments = arguments;
    }

    protected Values _call(
        Machine machine,
        Values  arguments
    ) throws SExpectedFunctionException {
        SExpr sexpr = arguments.at(0);

        try {
            machine.push(
                (SFunction)sexpr
            );
        } catch (ClassCastException e) {
            throw new SExpectedFunctionException(sexpr);
        }

        return _arguments;
    }


    protected String defaultString()
    {
        return
            "[call-with-values "
            + _arguments.toList()
            + "]";
    }
}
