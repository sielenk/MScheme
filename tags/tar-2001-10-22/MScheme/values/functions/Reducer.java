package MScheme.values.functions;

import MScheme.Value;

import MScheme.values.List;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


abstract class Reducer
{
    public final static String id
    = "$Id$";


    private final Value _initial;

    protected Reducer(Value initial)
    {
        _initial = initial;
    }

    protected abstract Value combine(Value fst, Value snd)
    throws RuntimeError, TypeError;


    public final Value reduceLeft(List list)
    throws RuntimeError, TypeError
    {
        if (list.isEmpty())
        {
            return _initial;
        }
        else
        {
            Value result = list.getHead();
	    
	        for (
                List tail = list.getTail();
                !tail.isEmpty();
                tail   = tail.getTail()
		    )
            {
                result = combine(result, tail.getHead());
            }

            return result;
        }
    }

    public final Value foldLeft(List list)
    throws RuntimeError, TypeError
    {
        Value result = _initial;
	
	    for (
            List tail = list;
            !tail.isEmpty();
            tail   = tail.getTail()
	    )
        {
            result = combine(result, tail.getHead());
        }

        return result;
    }

    private Value reduceRightHelper(List list)
    throws RuntimeError, TypeError
    {
        List tail = list.getTail();

        if (tail.isEmpty())
        {
            return list.getHead();
        }
        else
        {
            return combine(
                       list.getHead(),
                       reduceRightHelper(tail)
                   );
        }
    }

    public final Value reduceRight(List list)
    throws RuntimeError, TypeError
    {
        if (list.isEmpty())
        {
            return _initial;
        }
        else
        {
            return reduceRightHelper(list);
        }
    }

    public Value foldRight(List list)
    throws RuntimeError, TypeError
    {
        if (list.isEmpty())
        {
            return _initial;
        }
        else
        {
            return combine(
                       list.getHead(),
                       foldRight(list.getTail())
                   );
        }
    }
}
