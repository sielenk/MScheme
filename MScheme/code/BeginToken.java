package MScheme.code;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


// *** begin ***

final class CompiledSequence
    extends Code
{
    final private CodeList _sequence;

    CompiledSequence(CodeList sequence)
    { _sequence = sequence; }
    
    public Code executionStep(Machine machine)
    { return machine.handleSequence(_sequence); }
}

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

