package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;


public class ScmNumber
            extends ValueDefaultImplementations
{
    public final static String id
    = "$Id$";


    private int _value;

    private ScmNumber(int v)
    {
        _value = v;
    }

    public static ScmNumber create(int v)
    {
        return new ScmNumber(v);
    }

    // implementation of Value

    public boolean isScmNumber()
    {
        return true;
    }

    public ScmNumber toScmNumber()
    {
        return this;
    }

    public boolean eqv(Value other)
    {
        try
        {
            return isEqualTo((ScmNumber)other);
        }
        catch (ClassCastException e)
        { }

        return false;
    }

    public void write(Writer destination)
    throws IOException
    {
        destination.write("" + _value);
    }


    // number specific

    public int getInteger()
    {
        return _value;
    }


    public boolean isLessThan(ScmNumber other)
    {
        return _value < other._value;
    }

    public boolean isEqualTo(ScmNumber other)
    {
        return _value == other._value;
    }


    public ScmNumber negated()
    {
        return new ScmNumber(-_value);
    }

    public ScmNumber plus(ScmNumber other)
    {
        return new ScmNumber(_value + other._value);
    }

    public ScmNumber minus(ScmNumber other)
    {
        return new ScmNumber(_value - other._value);
    }

    public ScmNumber times(ScmNumber other)
    {
        return new ScmNumber(_value * other._value);
    }
}
