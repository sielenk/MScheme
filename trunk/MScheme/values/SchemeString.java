package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.exceptions.*;


public final class SchemeString
    extends Compound
{
    private char[] _string;


    private SchemeString(String value)
    {
        _string = new char[value.length()];
        value.getChars(
            0,
            value.length(),
            _string,
            0
        );
    }

    public static SchemeString create(String javaString)
    { return new SchemeString(javaString); }

    public static SchemeString create(Symbol schemeSymbol)
    {
        SchemeString result = new SchemeString(schemeSymbol.getKey());
        result.setConst();
        return result;
    }


    public String getJavaString()
    { return new String(_string); }


    // specialisation of Value

    public boolean isScmString()
    { return true; }

    public SchemeString toScmString()
    { return this; }


    // accessors

    public void set(int index, char c)
        throws InvalidStringIndexException, ImmutableException
    {
        modify();
        try {
            _string[index] = c;
        }
        catch (ArrayIndexOutOfBoundsException e) {
            throw new InvalidStringIndexException(this, index);
        }
    }

    public boolean equal(Value other)
    {
        try {
            SchemeString otherString = (SchemeString)other;
        
            return getJavaString().compareTo(
                otherString.getJavaString()
            ) == 0;
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
