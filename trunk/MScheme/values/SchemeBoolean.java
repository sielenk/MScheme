package MScheme.values;

import java.io.Writer;
import java.io.IOException;


public final class SchemeBoolean
    extends SelfEvaluatingValue
{
    final static SchemeBoolean True  = new SchemeBoolean();
    final static SchemeBoolean False = new SchemeBoolean();
    
    private SchemeBoolean() { }
        
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

