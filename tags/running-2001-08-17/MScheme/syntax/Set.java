package MScheme.syntax;

import MScheme.code.Code;
import MScheme.environment.StaticEnvironment;
import MScheme.environment.Reference;
import MScheme.values.List;
import MScheme.values.Symbol;

import MScheme.exceptions.CompileError;


final class Set
    extends Assign
{
    final static Syntax INSTANCE = new Set();
    
    protected Reference getReference(
        StaticEnvironment syntax,
        Symbol            symbol
    ) throws CompileError
    { return syntax.getCodeFor(symbol); }
}
