package MScheme.code;

import MScheme.Value;
import MScheme.Code;

import MScheme.machine.Registers;
import MScheme.machine.Continuation;
import MScheme.values.ValueFactory;
import MScheme.values.List;
import MScheme.values.Empty;

import MScheme.exceptions.*;


final class PushContinuation
    extends Continuation
{
    public final static String id
        = "$Id$";

    private final List     _done;
    private final CodeList _todo;


    private PushContinuation(
        Registers registers,
        List      done,
        CodeList  todo
    )
    {
        super(registers);
	    _done = done;
	    _todo = todo;
	}

    static Code prepareNext(
        Registers registers,
        List      done,
        CodeList  todo
    )
    {
        new PushContinuation(
            registers,
            done,
            todo.getTail()
        );
            
        return todo.getHead();
    }

    protected Code execute(Registers registers, Value value)
        throws RuntimeError, TypeError
    {
        if (_todo.isEmpty()) {
            return value.toFunction().call(registers, _done);
        } else {
            return prepareNext(
                registers,
                ValueFactory.prepend(value, _done),
                _todo
            );
        }
    }
}


public final class Application
    extends Code
{
    public final static String id
        = "$Id$";

    private final CodeList _permutedApplication;

    private Application(CodeList application)
    { _permutedApplication = application.getReversed(); }
    
    public static Code create(CodeList application)
    { return new Application(application); }
    
    public Code executionStep(Registers registers)
    {
        return PushContinuation.prepareNext(
            registers,
            Empty.create(),
            _permutedApplication
        );
    }
}