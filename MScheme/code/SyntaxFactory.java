package MScheme.code;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


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
        if (arguments.getHead().isPair()) {
            //    (define (f x y) (+ x y))
            // -> (define f (lambda (x y) (+ x y)))
            Value symbol  = arguments.getHead().toPair().getFirst();
            Value formals = arguments.getHead().toPair().getSecond();
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


// *** misc ***

public abstract class SyntaxFactory
{
    public static Token getBeginToken()
    { return BeginToken.INSTANCE; }
    
    public static Token getCondToken()
    { return CondToken.INSTANCE; }
    
    public static Token getSetToken()
    { return SetToken.INSTANCE; }
    
    public static Token getDefineToken()
    { return DefineToken.INSTANCE; }
    
    public static Token getLambdaToken()
    { return LambdaToken.INSTANCE; }
    
    public static Token getLetToken()
    { return LetToken.INSTANCE; }
    
    public static Token getIfToken()
    { return IfToken.INSTANCE; }
    
    public static Token getQuoteToken()
    { return QuoteToken.INSTANCE; }
}

