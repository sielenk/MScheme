package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;


public final class ScmBoolean
    extends Value
{
    private final static ScmBoolean TRUE  = new ScmBoolean();
    private final static ScmBoolean FALSE = new ScmBoolean();
    
    private ScmBoolean() { }


    public static ScmBoolean create(boolean flag)
    { return flag ? TRUE : FALSE; }

    public static ScmBoolean createFalse()
    { return FALSE; }

    public static ScmBoolean createTrue()
    { return TRUE; }


    public boolean isTrue()
    { return (this != FALSE); }
    
    public boolean isScmBoolean()
    { return true; }
    
    public void write(Writer destination)
        throws IOException
    {
        destination.write('#');
        if (isTrue()) {
            destination.write('t');
        } else {
            destination.write('f');
        }
    }
}

