package MScheme.environment;


import java.util.Hashtable;

import MScheme.expressions.SSymbol;

import MScheme.exceptions.SSymbolNotFoundException;
import MScheme.exceptions.SDuplicateSymbolException;

import MScheme.util.Values;


class Names
{
    // ***********************************************************************

    private Names     _parent;
    private Hashtable _bindings;
    private int       _level;

    // *** constructors ******************************************************

    protected Names(Names parent)
    {
        _bindings = new Hashtable();
        _parent   = parent;
        _level    = (parent == null) ? 0 : (parent.getLevel() + 1);
    }


    protected Names(
        Names  parent,
        Values symbols
    ) throws SDuplicateSymbolException {
        this(parent);

        int   len = symbols.getLength();

        for (int i = 0; i < len; i++) {
            define(
                (SSymbol)symbols.at(i)
            );
        }

        if (_bindings.size() != len) {
            throw new SDuplicateSymbolException(symbols.toList());
        }
    }

    // *** instance access ***************************************************

    public int getLevel () { return _level;  }
    public int getSize  () { return _bindings.size(); }

    public Names getParent() { return _parent; }

    // *** instance access ***************************************************

    protected int define(SSymbol symbol)
    {
        String  key = symbol.getKey();
        Integer i   = (Integer)_bindings.get(key);

        if (i != null) {
            // the symbol is already bound in the
            // current frame. This define is really
            // a set!.

            return i.intValue();
        } else {
            int index = _bindings.size();

            _bindings.put(key, new Integer(index));

            return index;
        }

    }


    protected Reference safeResolve(SSymbol symbol)
    {
        int   level   = _level;
        Names current = this;

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
