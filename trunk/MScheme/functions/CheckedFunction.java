package MScheme.functions;

import MScheme.util.Arity;
import MScheme.exceptions.*;
import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.values.List;
import MScheme.values.Function;


public abstract class CheckedFunction
    extends Function
{
    private final Arity   _arity;
    private final boolean _dispatch;
    
    protected CheckedFunction(Arity arity, boolean dispatch)
    { _arity = arity; _dispatch = dispatch; }
    
    protected CheckedFunction(Arity arity)
    { this(arity, true); }
    
    protected CheckedFunction(int arity)
    { this(Arity.exactly(arity)); }
    
    public Arity getArity()
    { return _arity; }
    

    private Code fail()
    { throw new RuntimeException("unimplemented Function called"); }

    protected Code checkedCall(
        Machine machine
    ) throws SchemeException
    { return fail(); }
    
    protected Code checkedCall(
        Machine machine,
        Value   first
    ) throws SchemeException
    { return fail(); }
    
    protected Code checkedCall(
        Machine machine,
        Value   first,
        Value   second
    ) throws SchemeException
    { return fail(); }
    
    protected Code checkedCall(
        Machine machine,
        Value   first,
        Value   second,
        Value   third
    ) throws SchemeException
    { return fail(); }
    
    protected Code checkedCall(
        Machine machine,
        List arguments
    ) throws SchemeException
    { return fail(); }


    // implementation of Function
    
    final public Code call(Machine machine, List arguments)
        throws SchemeException
    {
        int len = arguments.getLength();
        
        if (!getArity().isValid(len)) {
            throw new ArityException(arguments, getArity());
        }
        
        if (_dispatch) {
            switch (len) {
            case 0:
                return checkedCall(
                    machine
                );
        
            case 1:
                return checkedCall(
                    machine,
                    arguments.getHead()
                );
        
            case 2:
                return checkedCall(
                    machine,
                    arguments.getHead(),
                    arguments.getTail().getHead()
                );
        
            case 3:
                return checkedCall(
                    machine,
                    arguments.getHead(),
                    arguments.getTail().getHead(),
                    arguments.getTail().getTail().getHead()
                );
            }
        }
        
        return checkedCall(machine, arguments);
    }
}

