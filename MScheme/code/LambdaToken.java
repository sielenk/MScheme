package MScheme.code;

import java.io.Writer;
import java.io.IOException;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


// *** lambda ***

final class Closure
    extends CheckedFunction
{
    private final DynamicEnvironment _dynamicParent;
    private final StaticEnvironment  _compiledFormals;
    private final CodeList           _compiledBody;
    
    Closure(
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

final class CompiledLambda
    extends Code
{
    private final Arity             _arity;
    private final StaticEnvironment _compiledFormals;
    private final CodeList          _compiledBody;
    
    CompiledLambda(
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

final class LambdaToken
    extends Syntax
{
    final static Syntax INSTANCE = new LambdaToken();
    
    private LambdaToken()
    { super(Arity.atLeast(2)); }


    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    {
        Value rawFormals = arguments.getHead();
        List  body       = arguments.getTail();

        final List  formals;
        final Arity arity;
        
        if (rawFormals.isList()) {
            formals = rawFormals.toList();
            arity   = Arity.exactly(formals.getLength());
        } else {
            Pair head     = ValueFactory.createPair(null, rawFormals);
            Pair lastPair = head;
            int  minArity = 0;
        
            while (lastPair.getSecond().isPair()) {
                lastPair = lastPair.getSecond().toPair();
                minArity++;
            }
            
            lastPair.setSecond(
                ValueFactory.createList(
                    lastPair.getSecond().toSymbol()
                )
            );
            
            formals = head.getSecond().toList();
            arity   = Arity.atLeast(minArity);
        }

        StaticEnvironment
            compiledFormals = syntax.newChild(formals);
        CodeList
            compiledBody    = body.getCodeList(compiledFormals);
                    
        return new CompiledLambda(
            arity,
            compiledFormals,
            compiledBody
        );
    }
}

