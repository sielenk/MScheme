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
    
    protected CheckedFunction(boolean dispatch)
    { this(Arity.atLeast(0), dispatch); }
    
    protected CheckedFunction()
    { this(Arity.atLeast(0), false); }


    public Arity getArity()
    { return _arity; }
    

    private Value fail()
    { throw new RuntimeException("unimplemented Function called"); }


    protected Value checkedCall(
    ) throws SchemeException
    { return fail(); }

    protected Value checkedCall(
        Value fst
    ) throws SchemeException
    { return fail(); }

    protected Value checkedCall(
        Value fst,
        Value snd
    ) throws SchemeException
    { return fail(); }

    protected Value checkedCall(
        Value fst,
        Value snd,
        Value trd
    ) throws SchemeException
    { return fail(); }

    protected Value checkedCall(
        List  args
    ) throws SchemeException
    { return fail(); }


    protected Code checkedCall(
        Machine machine
    ) throws SchemeException
    { return machine.handleResult(checkedCall()); }
    
    protected Code checkedCall(
        Machine machine,
        Value   fst
    ) throws SchemeException
    { return machine.handleResult(checkedCall(fst)); }
    
    protected Code checkedCall(
        Machine machine,
        Value   fst,
        Value   snd
    ) throws SchemeException
    { return machine.handleResult(checkedCall(fst, snd)); }
    
    protected Code checkedCall(
        Machine machine,
        Value   fst,
        Value   snd,
        Value   trd
    ) throws SchemeException
    { return machine.handleResult(checkedCall(fst, snd, trd)); }


    protected Code checkedCall(
        Machine machine,
        List    args
    ) throws SchemeException
    { return machine.handleResult(checkedCall(args)); }


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
