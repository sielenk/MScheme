package MScheme.syntax;

import MScheme.util.Arity;
import MScheme.machine.Literal;
import MScheme.code.Code;
import MScheme.environment.StaticEnvironment;
import MScheme.values.Pair;
import MScheme.values.List;


// *** quote ***

final class QuoteToken
    extends Syntax
{
    final static Syntax INSTANCE = new QuoteToken();
    
    private QuoteToken()
    { super(Arity.exactly(1)); }
    
    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    )
    {
        return new Literal(
            ((Pair)arguments).getHead().setConst()
        );
    }
}
  
