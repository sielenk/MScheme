package MScheme.code;

import MScheme.machine.Continuation;
import MScheme.machine.Machine;
import MScheme.values.Value;


final class SequenceContinuation
    extends Continuation
{
    final private CodeList _todo;

    private SequenceContinuation(
        Machine  machine,
        CodeList todo
    )
    { super(machine); _todo = todo; }

    static Code prepareNext(
        Machine  machine,
        CodeList todo
    )
    {
        CodeList tail = todo.getTail();

        if (!tail.isEmpty()) {
            new SequenceContinuation(machine, tail);
        }

        return todo.getHead();
    }

    protected Code execute(Machine machine, Value value)
    { return prepareNext(machine, _todo); }
}


final public class CompiledSequence
    extends Code
{
    final private CodeList _sequence;

    public CompiledSequence(CodeList sequence)
    { _sequence = sequence; }

    public Code executionStep(Machine machine)
    { return SequenceContinuation.prepareNext(machine, _sequence); }
}
