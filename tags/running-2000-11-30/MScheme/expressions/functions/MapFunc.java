package MScheme.expressions.functions;


import MScheme.expressions.SExpr;
import MScheme.expressions.SBool;
import MScheme.expressions.SFunction;

import MScheme.machine.ValuePair;
import MScheme.machine.Values;
import MScheme.machine.ValuesFactory;
import MScheme.machine.ContinuationStack;

import MScheme.environment.Environment;


public class MapFunc extends Function
{
    private SFunction _func;
    private Values    _raw;
    private ValuePair _done;


    private MapFunc(
        SFunction func,
        Values    raw,
        ValuePair done
    ) {
        super(1, 1);
        _func = func;
        _raw  = raw;
        _done = done;
    }


    private MapFunc(
        SFunction func,
        Values    raw
    ) {
        this(func, raw, null);
    }



    private MapFunc(
        SFunction func,
        int       minArity,
        int       maxArity
    ) {
        super(minArity, maxArity);
        _func = func;
        _raw  = null;
        _done = null;
    }


    public MapFunc(
        SFunction func
    ) {
        this(func, 0, -1);
    }


    public static SFunction wrap(
        SFunction func,
        SFunction wrappee
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
        if (_raw == null) {
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
                    new ComposeFunc(
                        _func,
                        new MapFunc(
                            _func,
                            arguments.getTail()
                        )
                    )
                );

                return new Values(arguments.at(0));
            }
        } else if (_raw.getLength() > 0) {
            stack.push(
                environment,
                new ComposeFunc(
                    _func,
                    new MapFunc(
                        _func,
                        _raw.getTail(),
                        new ValuePair(
                            arguments.at(0),
                            _done
                        )
                    )
                )
            );

            return new Values(_raw.at(0));
        } else {
            ValuesFactory fab  = new ValuesFactory();
            ValuePair     pair = _done;

            fab.prepend(arguments.at(0));
            while (pair != null) {
                fab.prepend(pair.head);
                pair = pair.tail;
            }

            return fab.getValues();
        }
    }


    protected String defaultString()
    {
        return "[map " + _func + "]";
    }
}
