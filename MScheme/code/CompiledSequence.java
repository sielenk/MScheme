package MScheme.code;

import MScheme.machine.Machine;


final public class CompiledSequence
    extends Code
{
    final private CodeList _sequence;

    public CompiledSequence(CodeList sequence)
    { _sequence = sequence; }
    
    public Code executionStep(Machine machine)
    { return machine.handleSequence(_sequence); }
}

