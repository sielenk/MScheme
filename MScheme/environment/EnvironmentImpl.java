package MScheme.environment;


import java.util.Vector;

import MScheme.expressions.SExpr;
import MScheme.expressions.SEmpty;
import MScheme.expressions.SSymbol;

import MScheme.exceptions.SSymbolNotFoundException;

import MScheme.machine.Values;


class EnvironmentImpl
    extends    EnvironmentStubImpl
    implements Environment
{
    // ***********************************************************************

    private Vector[] _data;

    // ***********************************************************************

    private Vector initData()
    {
        int level = getLevel();

        _data = new Vector[level + 1];

        if (level > 0) {
            System.arraycopy(getParentImpl()._data, 0, _data, 0, level);
        }

        return _data[level] = new Vector(getSize());
    }


    protected EnvironmentImpl(
        EnvironmentImpl parent
    ) {
        super(parent);
        initData();
    }


    protected EnvironmentImpl(
        EnvironmentStubImpl stub,
        int                 minArity,
        boolean             allowMore,
        Values              values
    ) {
        super(stub);
        Vector data = initData();

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


    public Environment newChild()
    {
        return new EnvironmentImpl(this);
    }


    public EnvironmentStub newChildStub(
        Values symbols
    ) {
        EnvironmentStubImpl stub = new EnvironmentStubImpl(this);
        int                 len  = symbols.getLength();

        for (int i = 0; i < len; i++) {
            stub.define(
                (SSymbol)symbols.at(i)
            );
        }

        return stub;
    }

    // *** Envrionment access ************************************************

    public void define(SSymbol symbol, SExpr value)
    {
        int    index = define(symbol);
        Vector data  = _data[getLevel()];

        if (index == data.size()) {
            data.addElement(value);
        } else {
            data.setElementAt(value, index);
        }
    }


    public void set(SSymbol symbol, SExpr value)
        throws SSymbolNotFoundException
    {
        Reference ref = resolve(symbol);

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
        Reference ref  = resolve(symbol);

        return (SExpr)_data[
            ref.getLevel()
        ].get(
            ref.getIndex()
        );
    }

    // ***********************************************************************
}
