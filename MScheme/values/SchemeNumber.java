package MScheme.values;

import java.io.Writer;
import java.io.IOException;


public class SchemeNumber
    extends SelfEvaluatingValue
{
    private int _value;
    
    private SchemeNumber(int v)
    { _value = v; }
    
    public static SchemeNumber create(int v)
    { return new SchemeNumber(v); }
    
    // implementation of Value
    
    public boolean isNumber()
    { return true; }
    
    public SchemeNumber toNumber()
    { return this; }
    
    public boolean eqv(Value other)
    {
        try {
            SchemeNumber otherNumber = (SchemeNumber)other;
        
            return _value == otherNumber._value;
        }
        catch (ClassCastException e) { }
        
        return false;
    }

    public void write(Writer destination)
        throws IOException
    { destination.write("" + _value); }


    // number specific

    public int getInteger()
    { return _value; }


    public boolean isLessThan(SchemeNumber other)
    { return _value < other._value; }


    public SchemeNumber negated()
    { return new SchemeNumber(-_value); }

    public SchemeNumber plus(SchemeNumber other)
    { return new SchemeNumber(_value + other._value); }

    public SchemeNumber minus(SchemeNumber other)
    { return new SchemeNumber(_value - other._value); }

    public SchemeNumber times(SchemeNumber other)
    { return new SchemeNumber(_value * other._value); }
}
