package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Registers;
import MScheme.code.Code;

import MScheme.exceptions.*;


public abstract class Function
    extends Value
{
    // specialisation of Value

    final public boolean isFunction()
    { return true; }
    
    final public Function toFunction()
    { return this; }
    
    public void write(Writer destination)
        throws IOException
    { destination.write("[procedure]"); }


    public final static int checkArguments(Arity arity, List arguments)
        throws RuntimeError, TypeError
    {
        int len = arguments.getLength();

        if (!arity.isValid(len)) {
            throw new RuntimeArityError(arguments, arity);
        }

        return len;
    }


    // abstract function interface
    
    abstract public Code call(Registers registers, List arguments)
        throws RuntimeError, TypeError;
}
