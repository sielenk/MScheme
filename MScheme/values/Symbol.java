package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.Translator;
import MScheme.Code;

import MScheme.environment.Reference;
import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.*;


public final class Symbol
    extends Value
{
    public final static String id
        = "$Id$";


    private final String _javaString;

    private Symbol(String javaString)
    { _javaString = javaString.intern(); }

    public static Symbol create(String javaString)
    { return new Symbol(javaString); }

    public static Symbol create(ScmString schemeString)
    { return create(schemeString.getJavaString()); }

    private static int _index = 0;
	public static Symbol createUnique()
    { return create("[" + _index++ + "]"); }


    public String getJavaString()
    { return _javaString; }


    // specialisation/implementation of Value

    public boolean isSymbol()
    { return true; }

    public Symbol toSymbol()
    { return this; }


    public boolean eq(Value other)
    {
        try {
            Symbol otherSymbol = (Symbol)other;
            
            return getJavaString() == otherSymbol.getJavaString();
        }
        catch (ClassCastException e) { }

        return false;
    }


    public void write(Writer destination)
        throws IOException
    { destination.write(getJavaString()); }


    public Reference getReference(StaticEnvironment env)
        throws SymbolNotFoundException, UnexpectedSyntax
    { return env.getReferenceFor(this); }

    public Code getCode(StaticEnvironment env)
        throws SymbolNotFoundException, UnexpectedSyntax
    { return getReference(env); }

    public Translator getTranslator(StaticEnvironment env)
        throws SymbolNotFoundException
    { return env.getTranslatorFor(this); }
}
