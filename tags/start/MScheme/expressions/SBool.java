package MScheme.expressions;

public class SBool extends SExpr
{
    public final static SBool TRUE  = new SBool();
    public final static SBool FALSE = new SBool();

    private SBool() { }
}

