package MScheme.syntax;

import MScheme.util.Arity;

import MScheme.Value;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.machine.Machine;
import MScheme.code.CodeList;
import MScheme.code.Application;
import MScheme.environment.Environment;
import MScheme.environment.StaticEnvironment;
import MScheme.environment.Reference;
import MScheme.values.List;
import MScheme.values.Empty;
import MScheme.values.Pair;
import MScheme.values.Symbol;
import MScheme.values.InputPort;
import MScheme.functions.ApplyFunction;

import MScheme.exceptions.*;


final class Macro
    extends Syntax
{
    public final static String id
        = "$Id$";


    private final static Code
        _apply = ApplyFunction.INSTANCE.getLiteral();

    private final Code              _transformer;
    private final StaticEnvironment _definitionEnv;
    
    Macro(Code transformer, StaticEnvironment definitionEnv)
    {
        super(Arity.atLeast(0));
        _transformer   = transformer;
        _definitionEnv = definitionEnv;
    }
    
    protected Code checkedTranslate(
        StaticEnvironment usageEnv,
        List              arguments
    ) throws CompileError, TypeError
    {
        try {
            // (apply tranformer def_env use_env args)
    
            Pair result = new Machine(
                Environment.getImplementationEnvironment()
            ).execute(
                Application.create(
                    CodeList.prepend(
                        _apply,
                        CodeList.prepend(
                            _transformer,
                            CodeList.create(
                                _definitionEnv.getLiteral(),
                                usageEnv.getLiteral(),
                                arguments.getLiteral()
                            )
                        )
                    )
                )
            ).toPair();
            
            return
                result
                .getSecond()
                .getCode(
                    result
                    .getFirst()
                    .toStaticEnvironment()
                );
        }
        catch (RuntimeError e) {
            throw new CompileError(e.getCause());
        }
    }
}

final class DefineSyntax
    extends Syntax
{
    public final static String id
        = "$Id$";


    final static Syntax INSTANCE = new DefineSyntax();

    private DefineSyntax()
    { super(Arity.exactly(2)); }

    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws CompileError, TypeError
    {
        Symbol symbol = arguments.getHead().toSymbol();
        Value  value  = arguments.getTail().getHead();

        try {
            Macro macro = new Macro(
                new Machine(Environment.getImplementationEnvironment())
                    .evaluate(value)
                    .toFunction()
                    .getLiteral(),
                compilationEnv
            );

            compilationEnv.defineSyntax(symbol, macro);
            Environment
                .getImplementationEnvironment()
                .getStatic()
                .defineSyntax(symbol, macro);
        }
        catch (RuntimeError e) {
            throw new CompileError(e.getCause());
        }

        return Empty.create().getLiteral();
    }
}
