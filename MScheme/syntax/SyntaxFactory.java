package MScheme.syntax;


public abstract class SyntaxFactory
{
    public static Syntax getBeginToken()
    { return BeginToken.INSTANCE; }
    
    public static Syntax getCondToken()
    { return CondToken.INSTANCE; }
    
    public static Syntax getSetToken()
    { return SetToken.INSTANCE; }
    
    public static Syntax getDefineToken()
    { return DefineToken.INSTANCE; }
    
    public static Syntax getLambdaToken()
    { return LambdaToken.INSTANCE; }
    
    public static Syntax getLetToken()
    { return LetToken.INSTANCE; }
    
    public static Syntax getLetrecToken()
    { return LetrecToken.INSTANCE; }
    
    public static Syntax getIfToken()
    { return IfToken.INSTANCE; }
    
    public static Syntax getQuoteToken()
    { return QuoteToken.INSTANCE; }

    public static Syntax getQuasiquoteToken()
    { return QuasiquoteToken.INSTANCE; }
}
