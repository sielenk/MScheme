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
    implements Syntax
{
    public final static String id
        = "$Id$";


    private final static Code _apply
        = ApplyFunction.INSTANCE.getLiteral();

    private final Code              _transformer;
    private final StaticEnvironment _definitionEnv;

    Macro(Code transformer, StaticEnvironment definitionEnv)
    {
        _transformer   = transformer;
        _definitionEnv = definitionEnv;
    }

    public Code translate(
        StaticEnvironment usageEnv,
        List              arguments
    ) throws SchemeException
    {
        // (apply tranformer def_env use_env args)

        Pair result = Machine.execute(
            Application.create(
                CodeList.prepend(
                    _apply,
                    CodeList.create(
                        _transformer,
                        _definitionEnv.getLiteral(),
                        usageEnv.getLiteral(),
                        arguments.getLiteral()
                    )
                )
            ),
            Environment.getImplementationEnvironment()
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
}

final class DefineSyntax
    extends CheckedSyntax
{
    public final static String id
    = "$Id$";


    final static Syntax INSTANCE = new DefineSyntax();

    private DefineSyntax()
    {
        super(Arity.exactly(2));
    }

    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        Symbol symbol = arguments.getHead().toSymbol();
        Value  value  = arguments.getTail().getHead();

        Macro macro = new Macro(
            Machine.evaluate(
                value,
                Environment.getImplementationEnvironment()
            )
            .toFunction()
            .getLiteral(),
            compilationEnv
        );

        compilationEnv.defineSyntax(symbol, macro);
        Environment
        .getImplementationEnvironment()
        .getStatic()
        .defineSyntax(symbol, macro);

        return Empty.create().getLiteral();
    }
}
