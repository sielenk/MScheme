package MScheme.expressions;

public class SNumber extends SExpr
{
    private int _value;


    public SNumber(int value)
    {
        _value = value;
    }


    public SNumber add(SNumber other)
    { return new SNumber(_value + other._value); }


    public SNumber mul(SNumber other)
    { return new SNumber(_value * other._value); }


    protected String defaultString()
    {
        return "" + _value;
    }
}
