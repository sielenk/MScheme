package MScheme.syntax;

import MScheme.Value;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.util.Arity;
import MScheme.environment.StaticEnvironment;
import MScheme.values.List;

import MScheme.exceptions.TypeError;


// *** quote ***

final class Quote
    extends Syntax
{
    public final static String id
        = "$Id$";

    final static Syntax INSTANCE = new Quote();
    
    private Quote()
    { super(Arity.exactly(1)); }
    
    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws TypeError
    {
        Value v = arguments.getHead();
        v.setConst();
        return v.getLiteral();
    }
}