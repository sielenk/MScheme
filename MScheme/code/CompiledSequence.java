package MScheme.code;

import MScheme.machine.Continuation;
import MScheme.machine.Machine;
import MScheme.values.Value;


final class SequenceContinuation
    extends Continuation
{
    final private CodeList _unevaluatedTail;

    SequenceContinuation(
        Machine  machine,
        CodeList unevaluatedTail
    )
    {
        super(machine);
        _unevaluatedTail = unevaluatedTail;
    }

    protected Code execute(Machine machine, Value value)
    { return new CompiledSequence(_unevaluatedTail); }
}


final public class CompiledSequence
    extends Code
{
    final private CodeList _sequence;

    public CompiledSequence(CodeList sequence)
    { _sequence = sequence; }

    public Code executionStep(Machine machine)
    {
        CodeList tail = _sequence.getTail();

        if (!tail.isEmpty()) {
            new SequenceContinuation(machine, tail);
        }

        return _sequence.getHead();
    }
}
