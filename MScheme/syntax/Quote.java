package MScheme.syntax;

import MScheme.util.Arity;
import MScheme.code.Code;
import MScheme.environment.StaticEnvironment;
import MScheme.Value;
import MScheme.values.List;

import MScheme.exceptions.TypeError;


// *** quote ***

final class Quote
    extends Syntax
{
    final static Syntax INSTANCE = new Quote();
    
    private Quote()
    { super(Arity.exactly(1)); }
    
    protected Code checkedTranslate(
        StaticEnvironment syntax,
	    int               len,
        List              arguments
    ) throws TypeError
    {
        Value v = arguments.getHead();
        v.setConst();
        return v.getLiteral();
    }
}
