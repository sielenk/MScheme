package MScheme.expressions;

public class SEmpty extends SList
{
    public final static SEmpty INSTANCE = new SEmpty();
    
    private SEmpty() { }

    public SValues toValues()
    { return SValues.EMPTY; }
}
