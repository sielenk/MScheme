package MScheme.machine;


import MScheme.expressions.SExpr;


public class ValuesFactory
{
    private ValuePair _first;
    private ValuePair _last;
    private int       _length;


    public ValuesFactory()
    {
        _length = 0;
    }


    public void append(SExpr sexpr)
    {
        ValuePair pair = new ValuePair(sexpr);

        if (_length == 0) {
            _first = pair;
        } else {
            _last.tail = pair;
        }
        _last = pair;
        _length++;
    }


    public Values getValues()
    {
        if (_length == 0) {
            return Values.EMPTY;
        } else {
            Values result = new Values(_first, _length);

            _first  = _last.tail = null;
            _length = 0;

            return result;
        }
    }
}
