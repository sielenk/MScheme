package MScheme.util;


import MScheme.expressions.SExpr;
import MScheme.expressions.SList;
import MScheme.expressions.SValues;


public class ValuesFactory
{
    private ValuePair _first;
    private ValuePair _last;
    private int       _length;


    public ValuesFactory()
    {
        _length = 0;
    }


    public ValuesFactory prepend(SExpr sexpr)
    {
        ValuePair pair = new ValuePair(sexpr);

        if (_length == 0) {
            _last = pair;
        } else {
            pair.tail = _first;
        }
        _first = pair;
        _length++;

        return this;
    }


    public ValuesFactory append(SExpr sexpr)
    {
        ValuePair pair = new ValuePair(sexpr);

        if (_length == 0) {
            _first = pair;
        } else {
            _last.tail = pair;
        }
        _last = pair;
        _length++;

        return this;
    }


    public SValues getValues()
    {
        int length = _length;

        // reset the factory
        _length = 0;

        switch (length) {
        case 0:
            return SValues.EMPTY;

        case 1:
            return new SValues(_first.head);

        default:
            {
                SExpr[] data = new SExpr[length];
                int     i    = 0;

                do {
                    data[i++] = _first.head;
                    _first    = _first.tail;
                } while (i < length);

                return new SValues(data);
            }
        }
    }
}
