package MScheme.code;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


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
    
    public final Code translateArguments(
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

