package MScheme.machine;

import MScheme.values.*;
import MScheme.code.*;
import MScheme.exceptions.*;
import MScheme.functions.UnaryFunction;
import MScheme.environment.*;


class AssignmentContinuation
    extends Continuation
{
    private final Reference _binding;

    AssignmentContinuation(Machine machine, Reference binding)
    {
        super(machine);
        _binding = binding;
    }

    protected Code internalInvoke(Machine machine, Value evaluationResult)
    {
        machine.getEnvironment().assign(_binding, evaluationResult);
        return machine.handleResult(evaluationResult);
    }
}


class SelectContinuation
    extends Continuation
{
    private final Code _onTrue;
    private final Code _onFalse;

    SelectContinuation(Machine machine, Code onTrue, Code onFalse)
    {
        super(machine);
        _onTrue  = onTrue;
        _onFalse = onFalse;
    }

    protected Code internalInvoke(Machine machine, Value evaluationResult)
    { return evaluationResult.isFalse() ? _onFalse : _onTrue; }
}


class SequenceContinuation
    extends Continuation
{
    final private CodeList _unevaluatedTail;

    private SequenceContinuation(
        Machine  machine,
        CodeList unevaluatedTail
    )
    {
        super(machine);
        _unevaluatedTail = unevaluatedTail;
    }


    static Code handle(Machine machine, CodeList sequence)
    {
        CodeList tail = sequence.getTail();

        if (!tail.isEmpty()) {
            new SequenceContinuation(machine, tail);
        }

        return sequence.getHead();
    }
    
    protected Code internalInvoke(Machine machine, Value value)
    { return handle(machine, _unevaluatedTail); }
}


class PushContinuation
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

    static Code handle(Machine machine, CodeList application)
    {
        CodeList permutedApplication
            = application.getReversed();
    
        new PushContinuation(
            machine,
            ValueFactory.createList(),
            permutedApplication.getTail()
        );
        
        return permutedApplication.getHead();
    }

    private boolean allElementsEvaluated()
    { return _todo.isEmpty(); }
    
    private Code applyFunction(
        Machine machine,
        List    application
    ) throws RuntimeError, TypeError
    {
        Function func = application.getHead().toFunction();
        List     args = application.getTail();
        
        return func.call(machine, args);
    }

    private Code prepareNextElement(
        Machine machine,
        List    evaluatedPart
    )
    {
        new PushContinuation(
            machine,
            evaluatedPart,
            _todo.getTail()
        );
            
        return _todo.getHead();
    }
    
    protected Code internalInvoke(Machine machine, Value value)
        throws RuntimeError, TypeError
    {
        List evaluatedPart = ValueFactory.prepend(value, _done);

        if (allElementsEvaluated()) {
            return applyFunction     (machine, evaluatedPart);
        } else {
            return prepareNextElement(machine, evaluatedPart);
        }
    }
}


class AbortContinuation
    extends Continuation
{
    private Value _result;

    AbortContinuation(Machine machine)
    {
        super(machine);
        _result = null;
    }

    Value getResult()
    { return _result; }

    protected Code internalInvoke(Machine machine, Value evaluationResult)
    {
        _result = evaluationResult;
        return null;
    }
}


public class Machine
{
    private DynamicEnvironment _environment;
    private Continuation       _continuation;
    
    public Machine(DynamicEnvironment environment)
    {
        _environment  = environment;
        _continuation = null;
    }

    public Machine()
    { this(DynamicEnvironment.getSchemeReportEnvironment()); }


    public DynamicEnvironment getEnvironment()
    { return _environment; }

    public void setEnvironment(DynamicEnvironment newEnvironment)
    { _environment = newEnvironment; }
    

    public Continuation getContinuation()
    { return _continuation; }

    void setContinuation(Continuation newContinuation)
    { _continuation = newContinuation; }

    
    public Code handleDynamicWind(
        Function before,
        Function thunk,
        Function after
    ) throws RuntimeError, TypeError
    { return WindContinuation.handle(this, before, thunk, after); }

    public Code handleApplication(CodeList application)
    { return PushContinuation.handle(this, application); }
    
    public Code handleSequence(CodeList sequence)
    { return SequenceContinuation.handle(this, sequence); }
    
    public static Code handleResult(Value result)
    { return new Literal(result); }
    
    public void storeValueAt(Reference binding)
    { new AssignmentContinuation(this, binding); }

    public void select(Code onTrue, Code onFalse)
    { new SelectContinuation(this, onTrue, onFalse); }


    public Value evaluate(Value evaluatee)
        throws SchemeException
    {
        Code accumulator = evaluatee.getCode(
            getEnvironment().getStatic()
        );
        
        setContinuation(null);
        AbortContinuation abort = new AbortContinuation(this);
        
        while (accumulator != null) {
            accumulator = accumulator.executionStep(this);
        }

        return abort.getResult();
    }
}

