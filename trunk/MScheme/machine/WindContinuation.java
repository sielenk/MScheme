package MScheme.machine;

import MScheme.Value;
import MScheme.Code;

import MScheme.code.CodeList;
import MScheme.code.Sequence;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


final public class WindContinuation
    extends Continuation
{
    final private Code _before;
    final private Code _after;

    private WindContinuation(
        Registers registers,
        Code      before,
        Code      after
    )
    {
        super(registers);
        _before = before;
        _after  = after;
    }


    static public Code create(
        Registers registers,
        Code      before,
        Code      thunk,
        Code      after
    ) throws RuntimeError, TypeError
    {
        new WindContinuation(registers, before, after);
	
	    return Sequence.create(
	        CodeList.create(
		        before,
			    thunk
			)
        );
    }


    protected CodeList dynamicWindLeave(CodeList sequence)
    {
        return super.dynamicWindLeave(
	        CodeList.prepend(
    	        _after,
    	        sequence
		    )
		);
	}

    protected CodeList dynamicWindEnter(CodeList sequence)
    {
        return CodeList.prepend(
	        _before,
    	    super.dynamicWindEnter(
	            sequence
	        )
		);
	}


    protected Code execute(
        Registers registers,
        Value     value
    ) throws RuntimeError, TypeError
    {
	    return Sequence.create(
	        CodeList.create(
			    _after,
			    value.getLiteral()
		    )
	    );
    }
}
