package MScheme.machine;

import MScheme.Value;
import MScheme.Code;

import MScheme.code.CodeList;
import MScheme.code.Sequence;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public final class WindContinuation
    extends Continuation
{
    public final static String id
        = "$Id$";

    private final Code _before;
    private final Code _after;

    private WindContinuation(
        Registers state,
        Code      before,
        Code      after
    )
    {
        super(state);
        _before = before;
        _after  = after;
    }


    public static Code create(
        Registers state,
        Code      before,
        Code      thunk,
        Code      after
    ) throws RuntimeError, TypeError
    {
        new WindContinuation(state, before, after);
    
        return Sequence.create(
            CodeList.create(
                before,
                thunk
            )
        );
    }

    protected Code execute(
        Registers state,
        Value     result
    ) throws RuntimeError, TypeError
    {
        return Sequence.create(
            CodeList.create(
                _after,
                result.getLiteral()
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
}
