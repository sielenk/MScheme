package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;

import MScheme.exceptions.*;


public final class ScmString
    extends Compound
{
    public final static String id
        = "$Id$";


    private final char[]  _string;


    private ScmString(boolean isConst, int size, char fill)
    {
        super(isConst);
        _string = new char[size];
        for (int i = 0; i < size; ++i) {
            _string[i] = fill;
        }
    }

    private ScmString(boolean isConst, String value)
    {
        super(isConst);
        _string = new char[value.length()];
        value.getChars(
            0,
            value.length(),
            _string,
            0
        );
    }

    public static ScmString create(int size, char fill)
    { return new ScmString(false, size, fill); }

    public static ScmString create(String javaString)
    { return new ScmString(false, javaString); }

    public static ScmString createConst(String javaString)
    { return new ScmString(true, javaString); }

    public static ScmString create(Symbol schemeSymbol)
    { return createConst(schemeSymbol.getJavaString()); }


    public String getJavaString()
    { return new String(_string); }


    // specialisation of Value

    public boolean isScmString()
    { return true; }

    public ScmString toScmString()
    { return this; }


    // implementation of Compound

    protected Value getConstCopy()
    { return createConst(getJavaString()); }


    // accessors

    public int getLength()
    { return _string.length; }
    
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

    public char get(int index)
        throws InvalidStringIndexException
    {
        try {
            return _string[index];
        }
        catch (ArrayIndexOutOfBoundsException e) {
            throw new InvalidStringIndexException(this, index);
        }
    }

    public boolean equal(Value other)
    {
        try {
            ScmString otherString = (ScmString)other;
        
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
