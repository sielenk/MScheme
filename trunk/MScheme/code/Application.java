package MScheme.code;

import MScheme.machine.State;
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
        State    state,
        List     done,
        CodeList todo
    )
    { super(state); _done = done; _todo = todo; }

    static Code prepareNext(
        State    state,
        List     done,
        CodeList todo
    )
    {
        new PushContinuation(
            state,
            done,
            todo.getTail()
        );
            
        return todo.getHead();
    }

    protected Code execute(State state, Value value)
        throws RuntimeError, TypeError
    {
        if (_todo.isEmpty()) {
            return value.toFunction().call(state, _done);
        } else {
            return prepareNext(
                state,
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
    
    public Code executionStep(State state)
    {
        return PushContinuation.prepareNext(
            state,
            Empty.create(),
            _permutedApplication
        );
    }
}
