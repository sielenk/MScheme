package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.Code;

import MScheme.util.Arity;
import MScheme.machine.Registers;

import MScheme.exceptions.*;


public abstract class Function
            extends ValueDefaultImplementations
{
    public final static String id
    = "$Id$";


    // specialisation of Value

    public final boolean isFunction()
    {
        return true;
    }

    public final Function toFunction()
    {
        return this;
    }

    public void write(Writer destination)
    throws IOException
    {
        destination.write("#[procedure]");
    }


    public final static int checkArguments(Arity arity, List arguments)
    throws SchemeException
    {
        int len = arguments.getLength();

        if (!arity.isValid(len))
        {
            throw new RuntimeArityError(arguments, arity);
        }

        return len;
    }


    // abstract function interface

    public abstract Code call(Registers state, List arguments)
    throws SchemeException;
}
