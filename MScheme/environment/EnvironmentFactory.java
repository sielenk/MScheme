package MScheme.environment;


import MScheme.expressions.SExpr;

import MScheme.machine.Values;


public class EnvironmentFactory
{
    // ***********************************************************************

    public static Environment getEmpty()
    {
        return new EnvironmentImpl(null);
    }


    public static Environment fill(
        EnvironmentStub stub,
        int             minArity,
        boolean         allowMore,
        Values          values
    ) {
        return new EnvironmentImpl(
            (EnvironmentStubImpl)stub,
            minArity,
            allowMore,
            values
        );
    }

    // ***********************************************************************
}
