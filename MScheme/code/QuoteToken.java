package MScheme.code;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


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
        return new Quotation(
            ((Pair)arguments).getHead()
        );
    }
}

