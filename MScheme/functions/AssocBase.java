package MScheme.functions;

import MScheme.Value;
import MScheme.values.ScmBoolean;
import MScheme.List;
import MScheme.values.Pair;

import MScheme.exceptions.ListExpected;
import MScheme.exceptions.PairExpected;


abstract class AssocBase
            extends BinaryValueFunction
{
    public final static String id
    = "$Id$";


    protected abstract boolean equal(Value fst, Value snd);

    protected final Value checkedCall(
        Value key,
        Value values
    ) throws ListExpected, PairExpected
    {
        for (
            List tail = values.toList();
            !tail.isEmpty();
            tail = tail.getTail()
        )
        {
            Pair pair = tail.getHead().toPair();

            if (equal(key, pair.getFirst()))
            {
                return pair;
            }
        }

        return ScmBoolean.createFalse();
    }
}
