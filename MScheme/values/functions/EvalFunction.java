package MScheme.values.functions;

import MScheme.Value;
import MScheme.Code;

import MScheme.environment.Environment;

import MScheme.machine.Registers;

import MScheme.exceptions.*;


public final class EvalFunction
    extends BinaryFunction
{
    public final static String id
        = "$Id$";


    public final static EvalFunction INSTANCE = new EvalFunction();

    private EvalFunction()
    { }

    protected Code checkedCall(Registers state, Value fst, Value snd)
        throws SchemeException
    {
        Environment newEnv  = snd.toEnvironment();
        Code        newCode = fst.getCode(newEnv.getStatic());

        state.setEnvironment(newEnv);
        return newCode;
    }
}
