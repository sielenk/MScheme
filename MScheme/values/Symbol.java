package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.environment.StaticEnvironment;
import MScheme.environment.Token;
import MScheme.code.Code;

import MScheme.exceptions.SymbolNotFoundException;
import MScheme.exceptions.SyntaxException;


public final class Symbol
    extends Value
{
    final private String _symbol;
    
    
    private Symbol(String javaString)
    { _symbol = javaString.intern(); }
    
    public static Symbol create(String javaString)
    { return new Symbol(javaString); }

    public static Symbol create(SchemeString schemeString)
    { return create(schemeString.getJavaString()); }


    public String getKey()
    { return _symbol; }
    
    
    // specialisation/implementation of Value
    
    public boolean isSymbol()
    { return true; }
    
    public Symbol toSymbol()
    { return this; }
    
    
    public boolean eq(Value other)
    {
        try {
            Symbol otherSymbol = (Symbol)other;
        
            return getKey() == otherSymbol.getKey();
        }
        catch (ClassCastException e) { }
        
        return false;
    }
    
    
    public void write(Writer destination)
        throws IOException
    { destination.write(getKey()); }


    public Code getCode(StaticEnvironment env)
        throws SymbolNotFoundException, SyntaxException
    { return env.getCodeFor(this); }

    public Token getToken(StaticEnvironment env)
        throws SymbolNotFoundException
    { return env.getTokenFor(this); }
}
