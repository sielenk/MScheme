package MScheme.values;

import java.io.Writer;
import java.io.IOException;


public final class SchemeBoolean
    extends SelfEvaluatingValue
{
    public final static SchemeBoolean True  = new SchemeBoolean();
    public final static SchemeBoolean False = new SchemeBoolean();
    
    private SchemeBoolean() { }

    public static SchemeBoolean create(boolean flag)
    { return flag ? True : False; }

    public boolean isFalse()
    { return (this == False); }
    
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

