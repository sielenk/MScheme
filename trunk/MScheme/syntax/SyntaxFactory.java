package MScheme.syntax;

import MScheme.Syntax;


public abstract class SyntaxFactory
{
    public final static String id
    = "$Id$";


    public static Syntax getBeginToken()
    {
        return Begin.INSTANCE;
    }

    public static Syntax getSetToken()
    {
        return Set.INSTANCE;
    }

    public static Syntax getDefineToken()
    {
        return Define.INSTANCE;
    }

    public static Syntax getDefineSyntaxToken()
    {
        return DefineSyntax.INSTANCE;
    }

    public static Syntax getLambdaToken()
    {
        return Lambda.INSTANCE;
    }

    public static Syntax getLetToken()
    {
        return Let.INSTANCE;
    }

    public static Syntax getLetStarToken()
    {
        return LetStar.INSTANCE;
    }

    public static Syntax getLetrecToken()
    {
        return Letrec.INSTANCE;
    }

    public static Syntax getIfToken()
    {
        return If.INSTANCE;
    }

    public static Syntax getQuoteToken()
    {
        return Quote.INSTANCE;
    }
}
