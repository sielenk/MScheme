package MScheme.expressions;

final class ValuePair {
    SExpr     head;
    ValuePair tail;

    ValuePair(SExpr h)
    { head = h; }
}

public class SValues extends SExpr
{
    private ValuePair _data;
    private int       _length;

    public final static SValues EMPTY = new SValues(null, 0);

    SValues(ValuePair data, int length)
    {
	_data   = data;
	_length = length;
    }
}

class SValuesFactory {
    private ValuePair _first;
    private ValuePair _last;
    private int       _length;

    public SValuesFactory()
    {
	_length = 0;
    }

    public void append(SExpr sexpr)
    {
	if (_length == 0) {
	    _first = _last = new ValuePair(sexpr);
	} else {
	    _last = _last.tail = new ValuePair(sexpr);
	}
	_length++;
    }

    public SValues getValues()
    {
	if (_length == 0) {
	    return SValues.EMPTY;
	} else {
	    SValues result = new SValues(_first, _length);

	    _first  = _last.tail = null;
	    _length = 0;

	    return result;
	}
    }
}
