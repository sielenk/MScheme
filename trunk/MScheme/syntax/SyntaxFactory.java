package MScheme.syntax;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.code.CodeList;
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
        throws SchemeException
    {
        machine.getEnvironment().assign(_key, value);
        return machine.handleResult(value);
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
            ? syntax.getCodeFor(symbol)
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
            Symbol symbol  = arguments.getHead().toPair().getFirst ().toSymbol();
            Value  formals = arguments.getHead().toPair().getSecond();
            Value  body    = arguments.getTail();
            
            return CodeList.create(
                new Assigner(syntax.define(symbol)).getCode(syntax),
                SyntaxFactory.getLambdaToken().translateArguments(
                    syntax,
                    Pair.create(
                        formals,
                        body
                    )
                )
            );
        } else {
            return create(syntax, arguments, false);
        }
    }
}

final class SetToken
    extends Syntax
{
    final static Syntax INSTANCE = new SetToken();
    
    private SetToken()
    { super(Arity.exactly(2)); }
    
    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    { return Assigner.createSetter(syntax, arguments); }
}

final class DefineToken
    extends Syntax
{
    final static Syntax INSTANCE = new DefineToken();
    
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
    public static Syntax getBeginToken()
    { return BeginToken.INSTANCE; }
    
    public static Syntax getCondToken()
    { return CondToken.INSTANCE; }
    
    public static Syntax getSetToken()
    { return SetToken.INSTANCE; }
    
    public static Syntax getDefineToken()
    { return DefineToken.INSTANCE; }
    
    public static Syntax getLambdaToken()
    { return LambdaToken.INSTANCE; }
    
    public static Syntax getLetToken()
    { return LetToken.INSTANCE; }
    
    public static Syntax getIfToken()
    { return IfToken.INSTANCE; }
    
    public static Syntax getQuoteToken()
    { return QuoteToken.INSTANCE; }
}
