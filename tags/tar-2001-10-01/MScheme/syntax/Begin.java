package MScheme.syntax;

import MScheme.List;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.Sequence;
import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.SchemeException;


final class Begin
            extends Syntax
{
    public final static String id
    = "$Id$";


    final static Syntax INSTANCE = new Begin();

    private Begin()
    {
        super(Arity.atLeast(1));
    }

    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        return Sequence.create(
                   arguments.getCodeList(compilationEnv)
               );
    }
}