package MScheme.environment;

import java.util.Hashtable;
import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;

import MScheme.Value;
import MScheme.Code;
import MScheme.Syntax;
import MScheme.Token;

import MScheme.values.Symbol;
import MScheme.values.List;

import MScheme.exceptions.*;


public class StaticEnvironment
    extends Value
{
    // ***********************************************************************

    public void write(Writer destination)
        throws IOException
    { destination.write("[environment]"); }

    // ***********************************************************************

    private StaticEnvironment _parent;
    private Hashtable         _bindings;
    private int               _level;
    private int               _numberOfReferences;

    // *** constructors ******************************************************

    StaticEnvironment()
    { this(null); }
    
    StaticEnvironment(StaticEnvironment parent)
    {
        _bindings = new Hashtable();
        _parent   = parent;
        _level    = (parent == null) ? 0 : (parent.getLevel() + 1);
        _numberOfReferences = 0;
    }


    StaticEnvironment(StaticEnvironment parent, List symbols)
        throws CompileError, TypeError
    {
        this(parent);

        int symbolsDefined = 0;

        for (
            List tail = symbols;
            !tail.isEmpty();
            tail = tail.getTail())
        {
            define(tail.getHead().toSymbol());
            symbolsDefined++;
        }

        if (getSize() != symbolsDefined)
        {
            throw new DuplicateSymbolException(symbols.toValue());
        }
    }

    StaticEnvironment(StaticEnvironment parent, Symbol symbol)
        throws CompileError
    { this(parent); define(symbol); }

    // *** instance access ***************************************************

    int getLevel () { return _level; }
    int getSize  () { return _numberOfReferences; }

    // *** implementation of StaticEnvironment *******************************
    
    public StaticEnvironment getParent() { return _parent; }

    public StaticEnvironment newChild()
    { return new StaticEnvironment(this); }
    
    public StaticEnvironment newChild(List symbols)
        throws CompileError, TypeError
    { return new StaticEnvironment(this, symbols); }
    
    public StaticEnvironment newChild(Symbol symbol)
        throws CompileError
    { return new StaticEnvironment(this, symbol); }
    
    // *** instance access ***************************************************

    public Reference define(Symbol symbol)
        throws AlreadyBound
    {
        try {
            String    key = symbol.getKey();
            Reference ref = (Reference)_bindings.get(key);

            // if ref is != null
            // the symbol is already bound in the
            // current frame. This define is in fact
            // a lookup.
            if (ref == null) {
                ref = new Reference(
                    symbol,
                    getLevel(),
                    getSize()
                );
                
                _bindings.put(key, ref);
                _numberOfReferences++;
            }

            return ref;
        }
        catch (ClassCastException e) {
            throw new AlreadyBound(symbol);
        }
    }


    public void defineSyntax(Symbol symbol, Syntax value)
        throws AlreadyBound
    {
        String  key = symbol.getKey();

        {
            Object o = _bindings.get(key);
            if ((o != null) && !(o instanceof Syntax)) {
                throw new AlreadyBound(symbol);
            }
	    }

        _bindings.put(key, value);
    }


    private Token safeGetTokenFor(Symbol symbol)
    {
        for (
            StaticEnvironment current = this;
            current != null;
            current = current._parent
        ) {
            Token result = (Token)current._bindings.get(symbol.getKey());
        
            if (result != null) {
                return result;
            }
        }

        return null;
    }

    public Token getTokenFor(Symbol symbol)
        throws SymbolNotFoundException
    {
        Token result = safeGetTokenFor(symbol);

        if (result != null) {
            return result;
        } else {
            throw new SymbolNotFoundException(symbol);
        }
    }

    public Reference getCodeFor(Symbol key)
        throws SymbolNotFoundException, UnexpectedSyntax
    {
        try {
            return (Reference)getTokenFor(key);
        }
        catch (ClassCastException e) {
            throw new UnexpectedSyntax(key);
        }
    }

    public boolean isBound(Symbol key)
    { return safeGetTokenFor(key) != null; }

    // ***********************************************************************
}
