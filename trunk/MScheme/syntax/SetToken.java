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
    ) throws SymbolNotFoundException, SyntaxException
    { return syntax.getCodeFor(symbol); }

    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    { return create(syntax, arguments); }
}
