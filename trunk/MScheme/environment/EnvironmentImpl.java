package MScheme.environment;


import java.util.Vector;

import MScheme.expressions.SExpr;
import MScheme.expressions.SEmpty;
import MScheme.expressions.SSymbol;

import MScheme.exceptions.SSymbolNotFoundException;
import MScheme.exceptions.SDuplicateSymbolException;

import MScheme.machine.Values;


class EnvironmentImpl
    implements Environment
{
    // ***********************************************************************

    private Names    _names;
    private Vector[] _data;

    // ***********************************************************************

    private EnvironmentImpl(
        Names    names,
        Vector[] parentData
    ) {
        int level = (_names = names).getLevel();

        _data = new Vector[level + 1];

        if (level > 0) {
            System.arraycopy(
                parentData, 0,
                _data, 0,
                level
            );
        }

        (_data[level] = new Vector()).setSize(names.getSize());
    }


    protected EnvironmentImpl(
        EnvironmentImpl parent
    ) {
        this(
            new Names(parent._names),
            parent._data
        );
    }


    protected EnvironmentImpl(
        Names    names,
        Vector[] parentData,
        int      minArity,
        boolean  allowMore,
        Values   values
    ) {
        this(
            names,
            parentData
        );

        Vector data = _data[_names.getLevel()];

        for (int i = 0; i < minArity; i++) {
            data.setElementAt(
                values.at(i),
                i
            );
        }

        if (allowMore) {
            data.setElementAt(
                values.getTail(minArity).toList(),
                minArity
            );
        }
    }


    public EnvironmentImpl()
    {
        this(
             new Names(null),
             null
        );
    }


    public Environment getParent()
    {
        return new EnvironmentImpl(
            _names.getParent(),
            _data
        );
    }


    public Environment newChild()
    {
        return new EnvironmentImpl(this);
    }


    public EnvironmentStub newChildStub(
        Values symbols
    ) throws SDuplicateSymbolException {
        return new EnvironmentStubImpl(
            new Names(_names, symbols),
            _data
        );
    }

    // *** Envrionment access ************************************************

    public boolean defined(SSymbol symbol)
    {
        return _names.defined(symbol);
    }


    public void define(SSymbol symbol, SExpr value)
    {
        int    index = _names.define(symbol);
        Vector data  = _data[_names.getLevel()];

        if (index == data.size()) {
            data.addElement(value);
        } else {
            data.setElementAt(value, index);
        }
    }


    public void set(SSymbol symbol, SExpr value)
        throws SSymbolNotFoundException
    {
        Reference ref = _names.resolve(symbol);

        _data[
            ref.getLevel()
        ].setElementAt(
            value,
            ref.getIndex()
        );
    }


    public SExpr get(SSymbol symbol)
        throws SSymbolNotFoundException
    {
        Reference ref  = _names.resolve(symbol);

        return (SExpr)_data[
            ref.getLevel()
        ].get(
            ref.getIndex()
        );
    }

    // ***********************************************************************
}
