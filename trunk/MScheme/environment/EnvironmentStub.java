package MScheme.environment;


import MScheme.machine.Values;


public interface EnvironmentStub
{
    public Environment fill(
        int     minArity,
        boolean allowMore,
        Values  values
    );
}
