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
        Registers registers,
        CodeList  todo
    )
    { super(registers); _todo = todo; }

    static Code prepareNext(
        Registers registers,
        CodeList  todo
    )
    {
        CodeList tail = todo.getTail();

        if (!tail.isEmpty()) {
            new SequenceContinuation(registers, tail);
        }

        return todo.getHead();
    }

    protected Code execute(Registers registers, Value value)
    { return prepareNext(registers, _todo); }
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

    public Code executionStep(Registers registers)
    { return SequenceContinuation.prepareNext(registers, _sequence); }
}
