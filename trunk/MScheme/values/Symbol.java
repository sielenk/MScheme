package MScheme.values;

import java.util.WeakHashMap;
import java.lang.ref.WeakReference;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.Translator;
import MScheme.Code;

import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.*;


public final class Symbol
    extends Value
{
    final private static WeakHashMap _map = new WeakHashMap();

    final private String _key;

    private Symbol(String javaString)
    { _key = javaString; }

    public static Symbol create(String key)
    {
        final WeakReference ref   = (WeakReference)_map.get(key);
        Symbol              value = (ref != null) ? (Symbol)ref.get() : null;

        // value might be null, even if ref isn't.
        // This happens, if the wealky referenced value is already collected
        // but it's map entry is still in place.

        if (value == null) {
            value = new Symbol(key);
            //        v-  and  -^ have to be the same ...
            _map.put(key, new WeakReference(value));
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


    public String getKey()
    { return _key; }


    // specialisation/implementation of Value

    public boolean isSymbol()
    { return true; }

    public Symbol toSymbol()
    { return this; }


    public void write(Writer destination)
        throws IOException
    { destination.write(getKey()); }


    public Code getCode(StaticEnvironment env)
        throws SymbolNotFoundException, UnexpectedSyntax
    { return env.getCodeFor(this); }

    public Translator getTranslator(StaticEnvironment env)
        throws SymbolNotFoundException
    { return env.getTranslatorFor(this); }
}
