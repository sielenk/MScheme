package MScheme.environment;


import MScheme.machine.Values;


public interface EnvironmentStub
{
    Environment fill(
        int     minArity,
        boolean allowMore,
        Values  values
    );
}
