package MScheme.environment;


import MScheme.util.Values;


public interface EnvironmentStub
{
    Environment fill(
        int     minArity,
        boolean allowMore,
        Values  values
    );
}
