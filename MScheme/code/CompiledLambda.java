package MScheme.code;

import java.io.Writer;
import java.io.IOException;

import MScheme.Code;
import MScheme.Value;

import MScheme.values.List;

import MScheme.util.Arity;

import MScheme.machine.Registers;
import MScheme.machine.Result;

import MScheme.environment.Reference;
import MScheme.environment.StaticEnvironment;
import MScheme.environment.Environment;

import MScheme.functions.CheckedFunction;

import MScheme.exceptions.ListExpected;
import MScheme.exceptions.PairExpected;
import MScheme.exceptions.CompileError;
import MScheme.exceptions.TypeError;
import MScheme.exceptions.SchemeException;


public final class CompiledLambda
            extends Result
{
    public final static String id
    = "$Id$";


    private final Arity             _arity;
    private final StaticEnvironment _compiledFormals;
    private final Code              _compiledBody;

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
    {
        this(arity, compiledFormals, Sequence.create(compiledBody));
    }

    public CompiledLambda(
        Arity             arity,
        StaticEnvironment compiledFormals,
        List              body
    ) throws CompileError, TypeError
    {
        this(arity, compiledFormals, body.getCodeList(compiledFormals));
    }

    final class Closure
                extends CheckedFunction
    {
        public final static String id
        = "$Id$";


        private final Environment _enclosingEnvironment;

        public Closure(Environment enclosingEnvironment)
        {
            _enclosingEnvironment = enclosingEnvironment;
        }

        public void write(Writer destination)
        throws IOException
        {
            destination.write("[closure]");
        }

        public Arity getArity()
        {
            return _arity;
        }

        protected Code checkedCall(
            Registers state,
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

            state.setEnvironment(newEnvironment);

            return _compiledBody;
        }
    }

    protected Value getValue(Registers state)
    {
        return new Closure(state.getEnvironment());
    }


    public String toString()
    {
        return "LAMBDA[" + _compiledBody + ']';
    }
}
