package MScheme.code;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.Reference;
import MScheme.environment.StaticEnvironment;
import MScheme.environment.DynamicEnvironment;
import MScheme.functions.CheckedFunction;
import MScheme.values.List;

import MScheme.exceptions.ListExpected;
import MScheme.exceptions.CompileError;
import MScheme.exceptions.TypeError;


public final class CompiledLambda
    extends Code
{
    private final Arity             _arity;
    private final StaticEnvironment _compiledFormals;
    private final Code              _compiledBody;
    private       Reference         _self = null;
    
    public CompiledLambda(
        Arity             arity,
        StaticEnvironment compiledFormals,
        Code              compiledBody
    )
    {
        _arity           = arity;
        _compiledFormals = compiledFormals;
        _compiledBody    = compiledBody;
    }

    public CompiledLambda(
        Arity             arity,
        StaticEnvironment compiledFormals,
        CodeList          compiledBody
    )
    { this(arity, compiledFormals, Sequence.create(compiledBody)); }

    public CompiledLambda(
        Arity             arity,
        StaticEnvironment compiledFormals,
        List              body
    ) throws CompileError, TypeError
    { this(arity, compiledFormals, body.getCodeList(compiledFormals)); }

    public void setSelf(Reference self)
    { _self = self; }

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
	        DynamicEnvironment newEnvironment = 
                _dynamicParent.newChild(
                    _compiledFormals,
                    getArity(),
                    arguments
                );

            machine.setEnvironment(newEnvironment);

            if (_self != null) {
	            newEnvironment.assign(
		            _self,
			        this
		        );
	        }

            return _compiledBody;
        }
    }

    public Code executionStep(Machine machine)
    { return new Closure(machine.getEnvironment()).getLiteral(); }
}
