package MScheme.machine;

import MScheme.Value;
import MScheme.Code;

import MScheme.code.CodeList;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public abstract class Continuation
{
    public final static String id
        = "$Id$";

    private final int       _level;
    private final Registers _capturedRegisters;


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


    protected abstract Code execute(
        Registers registers,
        Value     value
    ) throws RuntimeError, TypeError;

    protected CodeList dynamicWindLeave(CodeList sequence)
    { return sequence; }
    
    protected CodeList dynamicWindEnter(CodeList sequence)
    { return sequence; }
}