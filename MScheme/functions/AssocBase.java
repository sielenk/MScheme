package MScheme.functions;

import MScheme.values.ValueFactory;
import MScheme.values.Value;
import MScheme.values.List;
import MScheme.values.Pair;

import MScheme.exceptions.ListExpectedException;


abstract class AssocBase
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
            Pair pair = tail.getHead().toPair();

            if (equal(key, pair.getHead())) {
                return pair;
            }

            tail = tail.getTail();
        }

        return ValueFactory.createFalse();
    }
}

