package MScheme.functions;

import MScheme.Value;
import MScheme.values.ScmBoolean;
import MScheme.List;

import MScheme.exceptions.ListExpected;
import MScheme.exceptions.PairExpected;


abstract class MemberBase
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
        List tail = values.toList();

        while (!tail.isEmpty())
        {
            if (equal(key, tail.getHead()))
            {
                return tail;
            }

            tail = tail.getTail();
        }

        return ScmBoolean.createFalse();
    }
}