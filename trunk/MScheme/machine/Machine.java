package MScheme.machine;

import MScheme.values.*;
import MScheme.code.*;
import MScheme.exceptions.*;
import MScheme.functions.UnaryFunction;
import MScheme.environment.*;


final class AbortException
    extends SchemeException
{
    public AbortException(Value result)
    { super(result); }
}

class AbortContinuation
    extends Continuation
{
    AbortContinuation(Machine machine)
    { super(machine); }

    protected Code internalInvoke(Machine machine, Value evaluationResult)
        throws AbortException
    {
        throw new AbortException(evaluationResult);
    }
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
    ) throws SchemeException
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
        throws SchemeException
    {
        List evaluatedPart = ValueFactory.prepend(value, _done);

        if (allElementsEvaluated()) {
            return applyFunction     (machine, evaluatedPart);
        } else {
            return prepareNextElement(machine, evaluatedPart);
        }
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
    {
        this(DynamicEnvironment.getEmpty());

        DynamicEnvironment dynamicBindings
            = getEnvironment();

        StaticEnvironment staticBindings
            = getEnvironment().getStatic();

        try {
            dynamicBindings.define(
                ValueFactory.createSymbol(">"),
                ValueFactory.createFunction("NumberGT")
            );
            dynamicBindings.define(
                ValueFactory.createSymbol("<"),
                ValueFactory.createFunction("NumberLT")
            );
            dynamicBindings.define(
                ValueFactory.createSymbol("+"),
                ValueFactory.createFunction("Plus")
            );
            dynamicBindings.define(
                ValueFactory.createSymbol("-"),
                ValueFactory.createFunction("Minus")
            );
            dynamicBindings.define(
                ValueFactory.createSymbol("*"),
                ValueFactory.createFunction("Times")
            );

            staticBindings.defineSyntax(
                ValueFactory.createSymbol("quote"),
                SyntaxFactory.getQuoteToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("cond"),
                SyntaxFactory.getCondToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("if"),
                SyntaxFactory.getIfToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("begin"),
                SyntaxFactory.getBeginToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("lambda"),
                SyntaxFactory.getLambdaToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("let"),
                SyntaxFactory.getLetToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("define"),
                SyntaxFactory.getDefineToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("set!"),
                SyntaxFactory.getSetToken()
            );
        }
        catch (SyntaxException e) {
            throw new RuntimeException(
                "unexpected SyntaxException in Machine.Machine()"
            );
        }
        catch (FunctionNotFoundException e) {
            throw new RuntimeException(
                "unexpected FunctionNotFoundException in Machine.Machine()"
            );
        }
    }


    public DynamicEnvironment getEnvironment()
    { return _environment; }

    public void setEnvironment(DynamicEnvironment newEnvironment)
    { _environment = newEnvironment; }
    

    Continuation getContinuation()
    { return _continuation; }

    void setContinuation(Continuation newContinuation)
    { _continuation = newContinuation; }

    public UnaryFunction getCurrentContinuation()
    { return _continuation.getFunction(); }
    
    
    public Code handleApplication(CodeList application)
    { return PushContinuation.handle(this, application); }
    
    public Code handleSequence(CodeList sequence)
    { return SequenceContinuation.handle(this, sequence); }
    
    public Code handleResult(Value result)
        throws SchemeException
    { return getContinuation().invoke(this, result); }
    
    
    public Value evaluate(Value evaluatee)
        throws SchemeException
    {
        Code accumulator = evaluatee.getCode(
            getEnvironment().getStatic()
        );
        
        new AbortContinuation(this);
        
        try {

            for (;;)
            {
                accumulator = accumulator.executionStep(this);
            }
        
        } catch (AbortException e) {
            return e.getCause();
        }
    }
}

