package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.exceptions.*;
import MScheme.machine.Machine;
import MScheme.code.Code;


public abstract class Function
    extends    SelfEvaluatingValue
{
    // specialisation of Value

    final public boolean isFunction()
    { return true; }
    
    final public Function toFunction()
    { return this; }
    
    public void write(Writer destination)
        throws IOException
    { destination.write("[procedure]"); }
    
    
    // abstract function interface
    
    abstract public Code call(Machine machine, List arguments)
        throws SchemeException;
}

