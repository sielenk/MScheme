package MScheme.environment;

import MScheme.Value;

import MScheme.machine.Registers;
import MScheme.machine.Result;
import MScheme.values.Symbol;

import MScheme.exceptions.RuntimeError;


public class Reference
    extends Result
{
    public final static String id
        = "$Id$";

    private final Symbol _symbol;
    private final int    _level;
    private final int    _index;

    Reference(Symbol symbol, int level, int index)
    {
        _symbol = symbol;
        _level  = level;
        _index  = index;
    }

    public Symbol getSymbol() { return _symbol; }

    int getLevel () { return _level;  }
    int getIndex () { return _index;  }


    protected Value getValue(Registers registers)
        throws RuntimeError
    { return registers.getEnvironment().lookup(this); }
}
