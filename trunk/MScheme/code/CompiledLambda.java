package MScheme.code;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.StaticEnvironment;
import MScheme.exceptions.SchemeException;
import MScheme.functions.Closure;


final public class CompiledLambda
    extends Code
{
    private final Arity             _arity;
    private final StaticEnvironment _compiledFormals;
    private final CodeList          _compiledBody;
    
    public CompiledLambda(
        Arity             arity,
        StaticEnvironment compiledFormals,
        CodeList          compiledBody
    )
    {
        _arity           = arity;
        _compiledFormals = compiledFormals;
        _compiledBody    = compiledBody;
    }

    public Code executionStep(Machine machine)
        throws SchemeException
    {
        return machine.handleResult(
            new Closure(
                _arity,
                machine.getEnvironment(),
                _compiledFormals,
                _compiledBody
            )
        );
    }
}
