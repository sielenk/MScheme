package MScheme.code;

import java.io.Writer;
import java.io.IOException;

import MScheme.Code;

import MScheme.util.Arity;
import MScheme.machine.Registers;
import MScheme.environment.Reference;
import MScheme.environment.StaticEnvironment;
import MScheme.environment.Environment;
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
        private final Environment _enclosingEnvironment;
    
        public Closure(Environment enclosingEnvironment)
        { _enclosingEnvironment = enclosingEnvironment; }

        public void write(Writer destination)
            throws IOException
        { destination.write("[closure]"); }

        protected Arity getArity()
        { return _arity; }

        protected Code checkedCall(
            Registers registers,
            int       length,
            List      arguments
        ) throws ListExpected
        {
	        Environment newEnvironment = 
                _enclosingEnvironment.newChild(
                    _compiledFormals,
                    getArity(),
                    arguments
                );

            registers.setEnvironment(newEnvironment);

            if (_self != null) {
	            newEnvironment.assign(
		            _self,
			        this
		        );
	        }

            return _compiledBody;
        }
    }

    public Code executionStep(Registers registers)
    { return new Closure(registers.getEnvironment()).getLiteral(); }
}
