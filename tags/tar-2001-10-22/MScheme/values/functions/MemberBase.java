package MScheme.values.functions;

import MScheme.Value;

import MScheme.values.ScmBoolean;
import MScheme.values.List;

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
        for (
            List tail = values.toList();
            !tail.isEmpty();
            tail = tail.getTail()
	    )
        {
            if (equal(key, tail.getHead()))
            {
                return tail;
            }
        }

        return ScmBoolean.createFalse();
    }
}
