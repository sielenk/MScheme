package MScheme.machine;

import MScheme.code.Code;
import MScheme.code.CodeList;
import MScheme.values.Value;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public abstract class Continuation
{
    final private int       _level;
    final private Registers _capturedRegisters;


    protected Continuation(Registers registers)
    {
        _capturedRegisters = new Registers(registers);

        _level = (getParent() != null) ? getParent()._level + 1 : 0;

        registers.setContinuation(this);
    }

    final int getLevel()
    { return _level; }

    final Continuation getParent()
    { return _capturedRegisters.getContinuation(); }

    final Code invoke(Registers registers, Value value)
        throws RuntimeError, TypeError
    {
        registers.assign(_capturedRegisters);
        return execute(registers, value);
    }


    abstract protected Code execute(
        Registers registers,
        Value     value
    ) throws RuntimeError, TypeError;

    protected CodeList dynamicWindLeave(CodeList sequence)
    { return sequence; }
    
    protected CodeList dynamicWindEnter(CodeList sequence)
    { return sequence; }
}
