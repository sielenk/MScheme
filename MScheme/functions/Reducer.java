package MScheme.functions;

import MScheme.values.Value;
import MScheme.values.List;

import MScheme.exceptions.SchemeException;


abstract class Reducer
{
    protected abstract Value initial()
        throws SchemeException;

    protected abstract Value combine(Value fst, Value snd)
        throws SchemeException;


    public final Value reduceLeft(List list)
        throws SchemeException
    {
        if (list.isEmpty()) {
            return initial();
        } else {
            Value result = list.getHead();
            List  tail   = list.getTail();

            while (!tail.isEmpty()) {
                result = combine(result, tail.getHead());
                tail   = tail.getTail();
            }

            return result;
        }
    }

    public final Value foldLeft(List list)
        throws SchemeException
    {
        Value result = initial();
        List  tail   = list;

        while (!tail.isEmpty()) {
            result = combine(result, tail.getHead());
            tail   = tail.getTail();
        }

        return result;
    }

    private Value reduceRightHelper(List list)
        throws SchemeException
    {
        List tail = list.getTail();

        if (tail.isEmpty()) {
            return list.getHead();
        } else {
            return combine(
                list.getHead(),
                reduceRightHelper(tail)
            );
        }
    }

    public final Value reduceRight(List list)
        throws SchemeException
    {
       if (list.isEmpty()) {
            return initial();
        } else {
            return reduceRightHelper(list);
        }
    }

    public Value foldRight(List list)
        throws SchemeException
    {
        if (list.isEmpty()) {
            return initial();
        } else {
            return combine(
                list.getHead(),
                foldRight(list.getTail())
            );
        }
    }
}

