package MScheme.expressions.functions;


import MScheme.expressions.SExpr;
import MScheme.expressions.SFunction;

import MScheme.exceptions.SException;
import MScheme.exceptions.SWrongArgumentCountException;

import MScheme.machine.Values;
import MScheme.machine.ContinuationStack;

import MScheme.environment.Environment;


public abstract class Function extends SFunction
{
    private int _minArity;
    private int _maxArity;


    protected Function(
        int minArity,
        int maxArity
    ) {
        _minArity = minArity;
        _maxArity = maxArity;
    }


    abstract protected Values _call(
        ContinuationStack stack,
        Environment       environment,
        Values            aruments
    ) throws SException;


    final public Values call(
        ContinuationStack stack,
        Environment       environment,
        Values            arguments
    ) throws SException {
        _checkArity(arguments);

        System.err.println(
            "call "
            + defaultString()
            + " with "
            + arguments.toList()
        );

        return _call(stack, environment, arguments);
    }


    public int getMinArity()
    {
        return _minArity;
    }


    public int getMaxArity()
    {
        return _maxArity;
    }


    private void _checkArity(
        Values arguments
    ) throws SException {
        int len = arguments.getLength();

        if (
            (len < _minArity)
            || ((_maxArity != -1) && (_maxArity < len))
        ) {
            throw new SWrongArgumentCountException(
                arguments.toList(),
                _minArity,
                _maxArity
            );
        }
    }
}
