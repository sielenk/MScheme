package MScheme.expressions;


public class SSymbol extends SExpr
{
    private String _symbol;
    private String _key;

    public SSymbol(String symbol)
    {
        _symbol =  symbol.intern();
        _key    = _symbol.toLowerCase().intern();
    }

    public String getKey()
    {
        return _key;
    }


    protected String defaultString()
    {
        return _symbol;
    }
}
