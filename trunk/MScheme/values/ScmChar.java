package MScheme.values;

import java.io.Writer;
import java.io.IOException;


public final class ScmChar
    extends Value
{
    private char _character;
    
    private ScmChar(char c)
    { _character = c; }

    public static ScmChar create(char c)
    { return new ScmChar(c); }

    public char getJavaChar()
    { return _character; }

    // specialisation of Value

    public boolean isChar()
    { return true; }

    public ScmChar toChar()
    { return this; }


    public boolean eqv(Value other)
    {
        try {
            ScmChar otherCharacter = (ScmChar)other;
        
            return _character == otherCharacter._character;
        }
        catch (ClassCastException e) { }
        
        return false;
    }
    
    public void write(Writer destination)
        throws IOException
    {
        destination.write("#\\");
        switch (getJavaChar()) {
        case ' ':
            destination.write("space");
            break;
        
        case '\n':
            destination.write("newline");
            break;
            
        default:
            destination.write(getJavaChar());
            break;
        }
    }

    public void display(Writer destination)
        throws IOException
    { destination.write(getJavaChar()); }
}

