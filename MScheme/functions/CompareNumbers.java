package MScheme.functions;

import MScheme.util.Arity;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.ValueFactory;
import MScheme.values.Value;
import MScheme.values.List;
import MScheme.values.SchemeNumber;

import MScheme.exceptions.SchemeException;


abstract class CompareNumbers
    extends CheckedFunction
{
    protected CompareNumbers()
    { super(Arity.atLeast(2), false); }

    protected abstract boolean compare(
        SchemeNumber fst,
        SchemeNumber snd
    );

    protected Code checkedCall(
        Machine machine,
        List    arguments
    ) throws SchemeException
    {
        boolean      result  = true;
        SchemeNumber current = arguments.getHead().toNumber();
        List         tail    = arguments.getTail();

        while (!tail.isEmpty()) {
            SchemeNumber next = tail.getHead().toNumber();

            result &= compare(current, next);

            current = next;
            tail    = tail.getTail();
        }

        return machine.handleResult(
            ValueFactory.createBool(result)
        );
    }
}

