package MScheme.functions;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.DynamicEnvironment;
import MScheme.environment.StaticEnvironment;
import MScheme.code.Code;
import MScheme.code.CodeList;
import MScheme.values.List;

import MScheme.exceptions.ListExpectedException;


final public class Closure
    extends CheckedFunction
{
    private final DynamicEnvironment _dynamicParent;
    private final StaticEnvironment  _compiledFormals;
    private final CodeList           _compiledBody;
    
    public Closure(
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

