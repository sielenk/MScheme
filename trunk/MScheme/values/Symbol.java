package MScheme.values;

import java.util.WeakHashMap;
import java.lang.ref.WeakReference;

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

    final private static WeakHashMap _map = new WeakHashMap();

    final private String _javaString;

    private Symbol(String javaString)
    { _javaString = javaString; }

    public static Symbol create(String javaString)
    {
        final WeakReference ref   = (WeakReference)_map.get(javaString);
        Symbol              value = (ref != null) ? (Symbol)ref.get() : null;

        // value might be null, even if ref isn't.
        // This happens, if the wealky referenced value is already collected
        // but it's map entry is still in place.

        if (value == null) {
            value = new Symbol(javaString);
            //        v-  and  -^ have to be the same ...
            _map.put(javaString, new WeakReference(value));
            // ... to ensure the collection of the wealky referenced
            // value before it's map entry
        }

        return value;
    }

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
