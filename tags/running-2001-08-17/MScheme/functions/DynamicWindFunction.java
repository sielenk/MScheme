package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.machine.WindContinuation;
import MScheme.code.Code;
import MScheme.code.CodeList;
import MScheme.code.Application;
import MScheme.values.ValueFactory;
import MScheme.values.Value;
import MScheme.values.Function;

import MScheme.exceptions.*;


public class DynamicWindFunction
    extends TernaryFunction
{
    public final static DynamicWindFunction INSTANCE
        = new DynamicWindFunction();


    protected Code checkedCall(
        Machine machine,
        Value   fst,
        Value   snd,
        Value   trd
    ) throws RuntimeError, TypeError
    {
        Code before = createCall(fst);
        Code thunk  = createCall(snd);
        Code after  = createCall(trd);

        return WindContinuation.create(machine, before, thunk, after);
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

