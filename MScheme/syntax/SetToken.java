package MScheme.syntax;

import MScheme.code.Code;
import MScheme.environment.StaticEnvironment;
import MScheme.environment.Reference;
import MScheme.values.List;
import MScheme.values.Symbol;

import MScheme.exceptions.*;


final class SetToken
    extends AssignToken
{
    final static Syntax INSTANCE = new SetToken();
    
    protected Reference getReference(
        StaticEnvironment syntax,
        Symbol            symbol
    ) throws SyntaxException
    { return syntax.getCodeFor(symbol); }
}
