package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.environment.StaticEnvironment;
import MScheme.environment.Token;
import MScheme.code.Code;

import MScheme.exceptions.*;


public final class Symbol
    extends Value
{
    final private String _symbol;
    
    
    private Symbol(String javaString)
    { _symbol = javaString.intern(); }
    
    public static Symbol create(String javaString)
    { return new Symbol(javaString); }

    private static int _index = 0;
	public static Symbol createUnique()
    { return new Symbol("[" + _index++ + "]"); }

    public static Symbol create(ScmString schemeString)
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
        throws SymbolNotFoundException, UnexpectedSyntax
    { return env.getCodeFor(this); }

    public Token getToken(StaticEnvironment env)
        throws SymbolNotFoundException
    { return env.getTokenFor(this); }
}
