package MScheme.expressions.functions;


import MScheme.expressions.SExpr;
import MScheme.expressions.SBool;

import MScheme.machine.ValuePair;
import MScheme.machine.Values;
import MScheme.machine.ValuesFactory;
import MScheme.machine.ContinuationStack;

import MScheme.environment.Environment;


class MapHelperFunc extends Function
{
    private Function  _func;
    private Values    _raw;
    private ValuePair _done;


    private MapHelperFunc(
        Function  func,
        Values    raw,
        ValuePair done
    ) {
        super(1, 1);
        _func = func;
        _raw  = raw;
        _done = done;
    }


    public MapHelperFunc(
        Function  func,
        Values    raw
    ) {
        this(func, raw, null);
    }


    protected Values _call(
        ContinuationStack stack,
        Environment       environment,
        Values            arguments
    ) {
        if (_raw.getLength() > 0) {
            stack.push(
                environment,
                new MapHelperFunc(
                    _func,
                    _raw.getTail(),
                    new ValuePair(
                        arguments.at(0),
                        _done
                    )
                )
            );

            stack.push(
                environment,
                _func
            );
            return new Values(_raw.at(0));
        } else {
            ValuesFactory fab = new ValuesFactory();

            fab.prepend(arguments.at(0));
            while (_done != null) {
                fab.prepend(_done.head);
                _done = _done.tail;
            }

            return fab.getValues();
        }
    }


    protected String defaultString()
    {
        return "[map helper]";
    }
}


public class MapFunc extends Function
{
    private Function  _func;


    public  MapFunc(
        Function func
    ) {
        super(0, -1);
        _func = func;
    }


    private MapFunc(
        Function func,
        int      minArity,
        int      maxArity
    ) {
        super(minArity, maxArity);
        _func = func;
    }


    public static Function wrap(
        Function func,
        Function wrappee
    ) {
        return new ComposeFunc(
            new MapFunc(
                func,
                wrappee.getMinArity(),
                wrappee.getMaxArity()
            ),
            wrappee
        );
    }


    protected Values _call(
        ContinuationStack stack,
        Environment       environment,
        Values            arguments
    ) {
        switch (arguments.getLength()) {
        case 0:
            return Values.EMPTY;

        case 1:
            stack.push(
                environment,
                _func
            );

            return arguments;

        default:
            stack.push(
                environment,
                new MapHelperFunc(
                    _func,
                    arguments.getTail()
                )
            );

            stack.push(
                environment,
                _func
            );

            return new Values(arguments.at(0));
        }
    }


    protected String defaultString()
    {
        return "[map " + _func + "]";
    }
}
