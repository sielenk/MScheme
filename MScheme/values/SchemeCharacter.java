package MScheme.values;

import java.io.Writer;
import java.io.IOException;


public final class SchemeCharacter
    extends SelfEvaluatingValue
{
    private char _character;
    
    public SchemeCharacter(char c)
    { _character = c; }
    
    public char getChar()
    { return _character; }

    // specialisation of Value

    public boolean isChar()
    { return true; }

    public boolean eqv(Value other)
    {
        try {
            SchemeCharacter otherCharacter = (SchemeCharacter)other;
        
            return _character == otherCharacter._character;
        }
        catch (ClassCastException e) { }
        
        return false;
    }
    
    public void write(Writer destination)
        throws IOException
    {
        destination.write("#\\");
        switch (getChar()) {
        case ' ':
            destination.write("space");
            break;
        
        case '\n':
            destination.write("newline");
            break;
            
        default:
            destination.write(getChar());
            break;
        }
    }

    public void display(Writer destination)
        throws IOException
    { destination.write(getChar()); }
}

