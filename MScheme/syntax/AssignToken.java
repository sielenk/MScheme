package MScheme.syntax;

import MScheme.util.Arity;
import MScheme.code.Code;
import MScheme.code.CompiledAssignment;
import MScheme.environment.StaticEnvironment;
import MScheme.environment.Reference;
import MScheme.values.Value;
import MScheme.values.List;
import MScheme.values.Symbol;

import MScheme.exceptions.*;


abstract class AssignToken
    extends Syntax
{
    protected AssignToken()
    { super(Arity.exactly(2)); }

    abstract protected Reference getReference(
        StaticEnvironment syntax,
        Symbol            symbol
    ) throws CompileError;

    protected Code checkedTranslate(
        StaticEnvironment syntax,
        List              arguments
    ) throws CompileError, TypeError
    {
        Symbol symbol = arguments.getHead().toSymbol();
        Value  value  = arguments.getTail().getHead();

        return new CompiledAssignment(
            getReference(syntax, symbol),
            value.getCode(syntax)
        );
    }
}
