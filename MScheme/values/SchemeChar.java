package MScheme.values;

import java.io.Writer;
import java.io.IOException;


public final class SchemeChar
    extends SelfEvaluatingValue
{
    private char _character;
    
    private SchemeChar(char c)
    { _character = c; }

    public static SchemeChar create(char c)
    { return new SchemeChar(c); }

    public char getJavaChar()
    { return _character; }

    // specialisation of Value

    public boolean isChar()
    { return true; }

    public SchemeChar toChar()
    { return this; }


    public boolean eqv(Value other)
    {
        try {
            SchemeChar otherCharacter = (SchemeChar)other;
        
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

