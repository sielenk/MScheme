package MScheme.values;


public abstract class Port
    extends ValueDefaultImplementations
{
    protected Port()
    { }

    public final boolean isPort()
    {
        return true;
    }
}
