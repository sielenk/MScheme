package MScheme.functions;

import MScheme.Value;
import MScheme.values.ScmBoolean;
import MScheme.values.List;

import MScheme.exceptions.ListExpected;


abstract class MemberBase
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
            if (equal(key, tail.getHead())) {
                return tail.toValue();
            }

            tail = tail.getTail();
        }

        return ScmBoolean.createFalse();
    }
}

