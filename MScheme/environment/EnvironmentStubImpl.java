package MScheme.environment;


import java.util.Vector;

import MScheme.machine.Values;


class EnvironmentStubImpl
    implements EnvironmentStub
{
    // ***********************************************************************

    private Names    _childNames;
    private Vector[] _data;

    // *** constructors ******************************************************

    protected EnvironmentStubImpl(
        Names    childNames,
        Vector[] data
    ) {
        _childNames = childNames;
        _data       = data;
    }

    // ***********************************************************************

    public Environment fill(
        int                 minArity,
        boolean             allowMore,
        Values              values
    ) {
        return new EnvironmentImpl(
            _childNames,
            _data,
            minArity,
            allowMore,
            values
        );
    }

    // ***********************************************************************
}
