package MScheme.values;

import java.io.Writer;
import java.io.IOException;


public final class SchemeBoolean
    extends Value
{
    private final static SchemeBoolean TRUE  = new SchemeBoolean();
    private final static SchemeBoolean FALSE = new SchemeBoolean();
    
    private SchemeBoolean() { }


    public static SchemeBoolean create(boolean flag)
    { return flag ? TRUE : FALSE; }

    public static SchemeBoolean createFalse()
    { return FALSE; }

    public static SchemeBoolean createTrue()
    { return TRUE; }


    public boolean isFalse()
    { return (this == FALSE); }
    
    public boolean isBoolean()
    { return true; }
    
    public void write(Writer destination)
        throws IOException
    {
        destination.write('#');
        if (isFalse()) {
            destination.write('f');
        } else {
            destination.write('t');
        }
    }
}

