package MScheme.code;

import MScheme.machine.Machine;
import MScheme.machine.Continuation;
import MScheme.values.ValueFactory;
import MScheme.values.Value;
import MScheme.values.List;
import MScheme.values.Empty;

import MScheme.exceptions.*;


final class PushContinuation
    extends Continuation
{
    final private List     _done;
    final private CodeList _todo;


    private PushContinuation(
        Machine  machine,
        List     done,
        CodeList todo
    )
    { super(machine); _done = done; _todo = todo; }

    static Code prepareNext(
        Machine  machine,
        List     done,
        CodeList todo
    )
    {
        new PushContinuation(
            machine,
            done,
            todo.getTail()
        );
            
        return todo.getHead();
    }

    protected Code execute(Machine machine, Value value)
        throws RuntimeError, TypeError
    {
        if (_todo.isEmpty()) {
            return value.toFunction().call(machine, _done);
        } else {
            return prepareNext(
                machine,
                ValueFactory.prepend(value, _done),
                _todo
            );
        }
    }
}


final public class Application
    extends Code
{
    final private CodeList _permutedApplication;

    private Application(CodeList application)
    { _permutedApplication = application.getReversed(); }
    
    public static Code create(CodeList application)
    { return new Application(application); }
    
    public Code executionStep(Machine machine)
    {
        return PushContinuation.prepareNext(
            machine,
            Empty.create(),
            _permutedApplication
        );
    }
}