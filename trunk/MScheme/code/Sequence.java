package MScheme.code;

import MScheme.Value;
import MScheme.Code;

import MScheme.machine.Continuation;
import MScheme.machine.Registers;


final class SequenceContinuation
    extends Continuation
{
    public final static String id
        = "$Id$";

    private final CodeList _todo;

    private SequenceContinuation(
        Registers state,
        CodeList  todo
    )
    { super(state); _todo = todo; }

    static Code prepareNext(
        Registers state,
        CodeList  todo
    )
    {
        CodeList tail = todo.getTail();

        if (!tail.isEmpty()) {
            new SequenceContinuation(state, tail);
        }

        return todo.getHead();
    }

    protected Code execute(Registers state, Value value)
    { return prepareNext(state, _todo); }
}


public final class Sequence
    extends Code
{
    public final static String id
        = "$Id$";

    private final CodeList _sequence;

    private Sequence(CodeList sequence)
    { _sequence = sequence; }

    public static Code create(CodeList sequence)
    {
        if (sequence.getTail().isEmpty()) {
            return sequence.getHead();
        } else {
            return new Sequence(sequence);
        }
    }

    public Code executionStep(Registers state)
    { return SequenceContinuation.prepareNext(state, _sequence); }
}
