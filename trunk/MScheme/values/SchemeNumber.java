package MScheme.values;

import java.io.Writer;
import java.io.IOException;


public class SchemeNumber
    extends SelfEvaluatingValue
{
    private int _value;
    
    public SchemeNumber(int v)
    { _value = v; }
    
    
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

    public SchemeNumber plus(SchemeNumber other)
    {
        return new SchemeNumber(
            _value + other._value
        );
    }

    public SchemeNumber times(SchemeNumber other)
    {
        return new SchemeNumber(
            _value * other._value
        );
    }
}

