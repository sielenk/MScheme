package MScheme.functions;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.DynamicEnvironment;
import MScheme.environment.StaticEnvironment;
import MScheme.code.Code;
import MScheme.values.List;

import MScheme.exceptions.ListExpected;


final public class Closure
    extends CheckedFunction
{
    private final DynamicEnvironment _dynamicParent;
    private final StaticEnvironment  _compiledFormals;
    private final Code               _compiledBody;
    
    public Closure(
        Arity              arity,
        DynamicEnvironment dynamicParent,
        StaticEnvironment  compiledFormals,
        Code               compiledBody
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
        throws ListExpected
    {
        machine.setEnvironment(
            _dynamicParent.newChild(
                _compiledFormals,
                getArity(),
                arguments
            )
        );
        
        return _compiledBody;
    }
}

