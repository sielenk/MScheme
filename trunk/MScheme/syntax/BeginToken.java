package MScheme.syntax;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.code.CompiledSequence;
import MScheme.environment.StaticEnvironment;
import MScheme.values.List;

import MScheme.exceptions.SchemeException;


final class BeginToken
    extends Syntax
{
    final static Syntax INSTANCE = new BeginToken();
    
    private BeginToken()
    { super(Arity.atLeast(1)); }
    
    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    {
        return new CompiledSequence(
            arguments.getCodeList(syntax)
        );
    }
}

