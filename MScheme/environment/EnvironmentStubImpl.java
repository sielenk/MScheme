package MScheme.environment;


import java.util.Hashtable;
import java.util.Enumeration;

import MScheme.expressions.SSymbol;
import MScheme.exceptions.SSymbolNotFoundException;


class EnvironmentStubImpl
    implements EnvironmentStub
{

    // ***********************************************************************

    private EnvironmentImpl _parent;
    private Hashtable       _bindings;
    private int             _level;

    // *** constructors ******************************************************

    protected EnvironmentStubImpl(EnvironmentStubImpl stub)
    {
        _bindings = stub._bindings;
        _parent   = stub._parent;
        _level    = stub._level;
    }


    protected EnvironmentStubImpl(EnvironmentImpl parent)
    {
        _bindings = new Hashtable();
        _parent   = parent;
        _level    = (parent == null) ? 0 : (parent.getLevel() + 1);
    }

    // *** instance access ***************************************************

    public int         getLevel () { return _level;  }
    public Environment getParent() { return _parent; }

    public int         getSize  () { return _bindings.size(); }

    // *** instance access ***************************************************

    protected EnvironmentImpl getParentImpl()
    {
        return _parent;
    }


    public int define(SSymbol symbol)
    {
        String key   = symbol.getKey();
        int    index = -1;

        Integer i = (Integer)_bindings.get(key);

        if (i != null) {
            // the symbol is already bound in the
            // current frame. This define is really
            // a set!.

            index = i.intValue();
        }

        if (index == -1) {
            index = _bindings.size();
            _bindings.put(key, new Integer(index));
        }

        return index;
    }


    protected Reference safeResolve(SSymbol symbol)
    {
        int                 level   = _level;
        EnvironmentStubImpl current = this;

        while (current != null) {
            String  key = symbol.getKey();
            Integer i   = (Integer)current._bindings.get(key);
            if (i != null) {
                Reference new_ref = new Reference(
                    current.getLevel(),
                    i.intValue()
                );

                return new_ref;
            } else {
                current = current._parent;
            }
        }

        return null;
    }


    protected Reference resolve(SSymbol symbol)
        throws SSymbolNotFoundException
    {
        Reference ref = safeResolve(symbol);

        if (ref != null) {
            return ref;
        } else {
            throw new SSymbolNotFoundException(symbol);
        }
    }


    public boolean defined(SSymbol symbol)
    {
        return (safeResolve(symbol) != null);
    }

    // ***********************************************************************
}
