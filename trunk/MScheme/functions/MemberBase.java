package MScheme.functions;

import MScheme.values.ValueFactory;
import MScheme.values.Value;
import MScheme.values.List;

import MScheme.exceptions.ListExpectedException;


abstract class MemberBase
    extends BinaryValueFunction
{
    protected abstract boolean equal(Value fst, Value snd);

    protected final Value checkedCall(
        Value key,
        Value values
    ) throws ListExpectedException
    {
        List tail = values.toList();

        while (!tail.isEmpty()) {
            if (equal(key, tail.getHead())) {
                return tail;
            }

            tail = tail.getTail();
        }

        return ValueFactory.createFalse();
    }
}

