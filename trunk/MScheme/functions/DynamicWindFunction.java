package MScheme.functions;

import MScheme.machine.Registers;
import MScheme.machine.WindContinuation;
import MScheme.Code;
import MScheme.code.CodeList;
import MScheme.code.Application;
import MScheme.values.ValueFactory;
import MScheme.Value;
import MScheme.values.Function;

import MScheme.exceptions.*;


public class DynamicWindFunction
    extends TernaryFunction
{
    public final static String id
        = "$Id$";


    public final static DynamicWindFunction INSTANCE
        = new DynamicWindFunction();


    protected Code checkedCall(
        Registers state,
        Value     fst,
        Value     snd,
        Value     trd
    ) throws RuntimeError, TypeError
    {
        Code before = createCall(fst);
        Code thunk  = createCall(snd);
        Code after  = createCall(trd);

        return WindContinuation.create(state, before, thunk, after);
    }
    
    private static Code createCall(Value v)
        throws FunctionExpected
    {
        return Application.create(
	        CodeList.create(
		        v.toFunction().getLiteral()
			)
	    );
    }
}
