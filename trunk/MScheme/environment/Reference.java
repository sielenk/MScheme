package MScheme.environment;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Symbol;

import MScheme.exceptions.RuntimeError;


public class Reference
    extends Code
{
    private final Symbol _symbol;
    private final int    _level;
    private final int    _index;

    public Reference(Symbol symbol, int level, int index)
    {
        _symbol = symbol;
        _level  = level;
        _index  = index;
    }

    public Symbol getSymbol() { return _symbol; }
    public int    getLevel () { return _level;  }
    public int    getIndex () { return _index;  }
    
    
    public Code executionStep(Machine machine)
        throws RuntimeError
    { return machine.getEnvironment().lookup(this).getLiteral(); }
}
