package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.environment.StaticEnvironment;
import MScheme.code.Code;
import MScheme.exceptions.SymbolNotFoundException;


public final class Symbol
    extends Value
{
    final private String _symbol;
    
    
    Symbol(String symbol)
    { _symbol = symbol.intern(); }
    
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
        throws SymbolNotFoundException
    { return env.getCodeFor(this); }
}

