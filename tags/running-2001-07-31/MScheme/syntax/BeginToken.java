package MScheme.syntax;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.code.CompiledSequence;
import MScheme.environment.StaticEnvironment;
import MScheme.values.List;

import MScheme.exceptions.CompileError;
import MScheme.exceptions.TypeError;

final class BeginToken
    extends Syntax
{
    final static Syntax INSTANCE = new BeginToken();
    
    private BeginToken()
    { super(Arity.atLeast(1)); }
    
    protected Code checkedTranslate(
        StaticEnvironment syntax,
	    int               len,
        List              arguments
    ) throws CompileError, TypeError
    {
        return CompiledSequence.create(
            arguments.getCodeList(syntax)
        );
    }
}
