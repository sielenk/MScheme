package MScheme.environment;


public class EnvironmentFactory
{
    // ***********************************************************************

    public static Environment getEmpty()
    {
        return new EnvironmentImpl();
    }

    // ***********************************************************************
}
