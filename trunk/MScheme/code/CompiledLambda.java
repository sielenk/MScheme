package MScheme.code;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.StaticEnvironment;
import MScheme.environment.DynamicEnvironment;
import MScheme.functions.CheckedFunction;
import MScheme.values.List;

import MScheme.exceptions.ListExpected;


public final class CompiledLambda
    extends Code
{
    private final Arity             _arity;
    private final StaticEnvironment _compiledFormals;
    private final Code              _compiledBody;
    
    public CompiledLambda(
        Arity             arity,
        StaticEnvironment compiledFormals,
        CodeList          compiledBody
    )
    {
        _arity           = arity;
        _compiledFormals = compiledFormals;
        _compiledBody    = new CompiledSequence(compiledBody);
    }

    final class Closure
        extends CheckedFunction
    {
        private final DynamicEnvironment _dynamicParent;
    
        public Closure(DynamicEnvironment dynamicParent)
        { _dynamicParent   = dynamicParent; }

        public void write(Writer destination)
            throws IOException
        { destination.write("[closure]"); }

        protected Arity getArity()
        { return _arity; }

        protected Code checkedCall(
            Machine machine,
            int     length,
            List    arguments
        ) throws ListExpected
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

    public Code executionStep(Machine machine)
    { return new Closure(machine.getEnvironment()).getLiteral(); }
}
