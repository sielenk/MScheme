package MScheme.syntax;

import MScheme.util.Arity;
import MScheme.machine.Literal;
import MScheme.code.Code;
import MScheme.environment.StaticEnvironment;
import MScheme.values.Pair;
import MScheme.values.List;

import MScheme.exceptions.TypeError;


// *** quote ***

final class QuoteToken
    extends Syntax
{
    final static Syntax INSTANCE = new QuoteToken();
    
    private QuoteToken()
    { super(Arity.exactly(1)); }
    
    protected Code checkedTranslate(
        StaticEnvironment syntax,
        List              arguments
    ) throws TypeError
    { return new Literal(arguments.getHead().setConst()); }
}
  
