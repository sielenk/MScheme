package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class SymbolToStringFunction
    extends UnaryValueFunction
{
    public final static SymbolToStringFunction INSTANCE
        = new SymbolToStringFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws SchemeException
    {
        Value result = ValueFactory.createString(
            argument.toSymbol().getKey()
        );

        result.setLiteral();

        return result;
    }
}

