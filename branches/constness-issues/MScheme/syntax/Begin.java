package MScheme.syntax;

import MScheme.Code;
import MScheme.Syntax;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.Sequence;
import MScheme.environment.StaticEnvironment;
import MScheme.values.List;

import MScheme.exceptions.CompileError;
import MScheme.exceptions.TypeError;

final class Begin
    extends Syntax
{
    public final static String id
        = "$Id$";

    final static Syntax INSTANCE = new Begin();
    
    private Begin()
    { super(Arity.atLeast(1)); }
    
    protected Code checkedTranslate(
        StaticEnvironment syntax,
        List              arguments
    ) throws CompileError, TypeError
    {
        return Sequence.create(
            arguments.getCodeList(syntax)
        );
    }
}

