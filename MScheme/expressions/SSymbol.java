package MScheme.expressions;


public class SSymbol extends SExpr
{
    private String _symbol;

    public SSymbol(String symbol)
    {
        _symbol = symbol.intern();
    }

    public String getString()
    {
        return _symbol;
    }
}
