package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.exceptions.ImmutableException;


public final class SchemeString
    extends SelfEvaluatingValue
{
    private String  _string;
    private boolean _isLiteral = false;

    
    private SchemeString(String value)
    { _string = value; }

    public static SchemeString create(String javaString)
    { return new SchemeString(javaString); }

    public static SchemeString create(Symbol schemeSymbol)
    {
        SchemeString result = new SchemeString(schemeSymbol.getKey());
        result._isLiteral = true;
        return result;
    }


    public Value setLiteral()
    { _isLiteral = true; return this; }
     
    public String getJavaString()
    { return _string; }
    
    
    // specialisation of Value
    
    public boolean isString()
    { return true; }

    public SchemeString toScmString()
    { return this; }


    // accessors

    public void set(int index, char c)
        throws ImmutableException
    {
        if (_isLiteral) {
            throw new ImmutableException(this);
        }
    }

    public boolean equal(Value other)
    {
        try {
            SchemeString otherString = (SchemeString)other;
        
            return _string.compareTo(otherString._string) == 0;
        }
        catch (ClassCastException e) { }
        
        return false;
    }

    public void write(Writer destination)
        throws IOException
    {
        String str = getJavaString();
            
        destination.write('"');
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            switch (c) {
            case '\n':
                destination.write("\\n");
                break;
                
            case '"':
                destination.write("\\\"");
                break;
                
            default:
                destination.write(c);
                break;
            }
        }
        destination.write('"');
    }
    
    public void display(Writer destination)
        throws IOException
    { destination.write(getJavaString()); }
}

