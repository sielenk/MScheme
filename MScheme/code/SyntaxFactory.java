package MScheme.code;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


// *** quote ***

final class QuoteToken
    extends Token
{
    final static Token INSTANCE = new QuoteToken();
    
    private QuoteToken()
    { super(Arity.exactly(1)); }
    
    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    )
    {
        return new Quotation(
            ((Pair)arguments).getHead()
        );
    }
}


// *** set! *** define ***

final class Assigner
    extends UnaryFunction
{
    private final Reference _key;
    
    private Assigner(Reference key)
    { _key = key; }


    protected Code checkedCall(Machine machine, Value value)
    {
        machine.getEnvironment().assign(_key, value);
        return new Quotation(value);
    }


    private static Code create(
        StaticEnvironment syntax,
        List              arguments,
        boolean           isSetter
    ) throws SchemeException
    {
        Symbol symbol = arguments.getHead().toSymbol();
        Value  value  = arguments.getTail().getHead();
        
        Reference reference =
            isSetter
            ? syntax.getReferenceFor(symbol)
            : syntax.define(symbol);
    
        Function assigner = new Assigner(reference);
        
        return CodeList.create(
            assigner.getCode(syntax),
            value   .getCode(syntax)
        );
    }
    
    public static Code createSetter(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    { return create(syntax, arguments, true); }
    
    public static Code createDefiner(
        StaticEnvironment syntax,
        List              arguments
    )
        throws SchemeException
    {
        if (arguments.getHead().isList()) {
            //    (define (f x y) (+ x y))
            // -> (define f (lambda (x y) (+ x y)))
            Value symbol  = arguments.getHead().toList().getHead();
            Value formals = arguments.getHead().toList().getTail();
            Value body    = arguments.getTail();
            
            return create(
                syntax,
                ValueFactory.createList(
                    symbol,
                    ValueFactory.createPair(
                        SyntaxFactory.getLambdaToken().getValue(),
                        ValueFactory.createPair(
                            formals,
                            body
                        )
                    )
                ),
                false
            );
        } else {
            return create(syntax, arguments, false);
        }
    }
}

final class SetToken
    extends Token
{
    final static Token INSTANCE = new SetToken();
    
    private SetToken()
    { super(Arity.exactly(2)); }
    
    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    { return Assigner.createSetter(syntax, arguments); }
}

final class DefineToken
    extends Token
{
    final static Token INSTANCE = new DefineToken();
    
    private DefineToken()
    { super(Arity.exactly(2)); }
    
    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    { return Assigner.createDefiner(syntax, arguments); }
}


// *** begin ***

final class CompiledSequence
    extends Code
{
    final private CodeList _sequence;

    CompiledSequence(CodeList sequence)
    { _sequence = sequence; }
    
    public Code executionStep(Machine machine)
    { return machine.handleSequence(_sequence); }
}

final class BeginToken
    extends Token
{
    final static Token INSTANCE = new BeginToken();
    
    private BeginToken()
    { super(Arity.atLeast(1)); }
    
    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    {
        return new CompiledSequence(
            arguments.getCodeList(syntax)
        );
    }
}


// *** lambda ***

final class Closure
    extends CheckedFunction
{
    private final DynamicEnvironment _dynamicParent;
    private final StaticEnvironment  _compiledFormals;
    private final CodeList           _compiledBody;
    
    Closure(
        Arity              arity,
        DynamicEnvironment dynamicParent,
        StaticEnvironment  compiledFormals,
        CodeList           compiledBody
    )
    {
        super(arity, false);
        _dynamicParent   = dynamicParent;
        _compiledFormals = compiledFormals;
        _compiledBody    = compiledBody;
    }

    public void put(Writer destination, boolean doDisplay)
        throws IOException
    { destination.write("[closure]"); }
    
    protected Code checkedCall(Machine machine, List arguments)
        throws ListExpectedException
    {
        machine.setEnvironment(
            _dynamicParent.newChild(
                _compiledFormals,
                getArity(),
                arguments
            )
        );
        
        return machine.handleSequence(_compiledBody);
    }
}

final class CompiledLambda
    extends Code
{
    private final Arity             _arity;
    private final StaticEnvironment _compiledFormals;
    private final CodeList          _compiledBody;
    
    CompiledLambda(
        Arity             arity,
        StaticEnvironment compiledFormals,
        CodeList          compiledBody
    )
    {
        _arity           = arity;
        _compiledFormals = compiledFormals;
        _compiledBody    = compiledBody;
    }

    public Code executionStep(Machine machine)
        throws SchemeException
    {
        return machine.handleResult(
            new Closure(
                _arity,
                machine.getEnvironment(),
                _compiledFormals,
                _compiledBody
            )
        );
    }
}

final class LambdaToken
    extends Token
{
    final static Token INSTANCE = new LambdaToken();
    
    private LambdaToken()
    { super(Arity.atLeast(2)); }


    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    {
        Value rawFormals = arguments.getHead();
        List  body       = arguments.getTail();

        final List  formals;
        final Arity arity;
        
        if (rawFormals.isList()) {
            formals = rawFormals.toList();
            arity   = Arity.exactly(formals.getLength());
        } else {
            Pair head     = ValueFactory.createPair(null, rawFormals);
            Pair lastPair = head;
            int  minArity = 0;
        
            while (lastPair.getSecond().isPair()) {
                lastPair = lastPair.getSecond().toPair();
                minArity++;
            }
            
            lastPair.setSecond(
                ValueFactory.createList(
                    lastPair.getSecond().toSymbol()
                )
            );
            
            formals = head.getSecond().toList();
            arity   = Arity.atLeast(minArity);
        }

        StaticEnvironment
            compiledFormals = syntax.newChild(formals);
        CodeList
            compiledBody    = body.getCodeList(compiledFormals);
                    
        return new CompiledLambda(
            arity,
            compiledFormals,
            compiledBody
        );
    }
}


// *** if ***

final class Selector
    extends UnaryFunction
{
    private final Code _onTrue;
    private final Code _onFalse;

    Selector(Code onTrue, Code onFalse)
    { _onTrue = onTrue; _onFalse = onFalse; }

    protected Code checkedCall(Machine machine, Value flag)
    { return flag.isFalse() ? _onFalse : _onTrue; }
}

final class IfToken
    extends Token
{
    final static Token INSTANCE = new IfToken();
    
    private IfToken()
    { super(Arity.inRange(2, 3)); }
    
    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    {
        Value flag    = arguments.getHead();
        Value onTrue  = arguments.getTail().getHead();
        Value onFalse =
            (arguments.getLength() == 2)
            ? ValueFactory.createFalse()
            : arguments.getTail().getTail().getHead();
            
        Function selector = new Selector(
            onTrue .getCode(syntax),
            onFalse.getCode(syntax)
        );

        return CodeList.create(
            selector.getCode(syntax),
            flag    .getCode(syntax)
        );
    }
}


// *** misc ***

abstract class Token
    extends Syntax
{
    private final Arity _arity;
    
    protected Token(Arity arity)
    { _arity = arity; }
    
    protected abstract Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException;
    
    public final Code transform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    {
        if (!_arity.isValid(arguments.getLength())) {
            throw new ArityException(arguments, _arity);
        }

        return checkedTransform(syntax, arguments);
    }


    private class TokenValue
        extends Value
    {
        public void write(Writer destination)
            throws IOException
        { destination.write("[syntax]"); }
    
        public Code getCode(StaticEnvironment syntax)
        { return Token.this; }
    }
    
    private final TokenValue _value = new TokenValue();

    Value getValue()
    { return _value; }
}


public abstract class SyntaxFactory
{
    public static Token getBeginToken()
    { return BeginToken.INSTANCE; }
    
    public static Token getSetToken()
    { return SetToken.INSTANCE; }
    
    public static Token getDefineToken()
    { return DefineToken.INSTANCE; }
    
    public static Token getLambdaToken()
    { return LambdaToken.INSTANCE; }
    
    public static Token getIfToken()
    { return IfToken.INSTANCE; }
    
    public static Token getQuoteToken()
    { return QuoteToken.INSTANCE; }
}

