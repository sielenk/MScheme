package MScheme.code;

import MScheme.machine.Continuation;
import MScheme.machine.State;
import MScheme.values.Value;


final class SequenceContinuation
    extends Continuation
{
    final private CodeList _todo;

    private SequenceContinuation(
        State    state,
        CodeList todo
    )
    { super(state); _todo = todo; }

    static Code prepareNext(
        State    state,
        CodeList todo
    )
    {
        CodeList tail = todo.getTail();

        if (!tail.isEmpty()) {
            new SequenceContinuation(state, tail);
        }

        return todo.getHead();
    }

    protected Code execute(State state, Value value)
    { return prepareNext(state, _todo); }
}


final public class Sequence
    extends Code
{
    final private CodeList _sequence;

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

    public Code executionStep(State state)
    { return SequenceContinuation.prepareNext(state, _sequence); }
}
