package MScheme.functions;

import MScheme.Value;
import MScheme.values.ScmBoolean;
import MScheme.values.List;
import MScheme.values.Pair;

import MScheme.exceptions.ListExpected;


abstract class AssocBase
    extends BinaryValueFunction
{
    public final static String id
        = "$Id$";

    protected abstract boolean equal(Value fst, Value snd);

    protected final Value checkedCall(
        Value key,
        Value values
    ) throws ListExpected
    {
        List tail = values.toList();

        while (!tail.isEmpty()) {
            Pair pair = tail.getHead().toPair();

            if (equal(key, pair.getHead())) {
                return pair;
            }

            tail = tail.getTail();
        }

        return ScmBoolean.createFalse();
    }
}
