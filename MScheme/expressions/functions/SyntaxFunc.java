package MScheme.expressions.functions;


import MScheme.expressions.SExpr;
import MScheme.expressions.SPair;
import MScheme.expressions.SEmpty;
import MScheme.expressions.SBool;
import MScheme.expressions.SSymbol;

import MScheme.machine.Values;
import MScheme.machine.ValuesFactory;
import MScheme.machine.ContinuationStack;

import MScheme.environment.Environment;
import MScheme.environment.EnvironmentStub;

import MScheme.exceptions.SException;
import MScheme.exceptions.SExpectedSymbolException;
import MScheme.exceptions.SImproperFormalsException;


class SelectFunc extends Function
{
    private SExpr _trueExpr;
    private SExpr _falseExpr;

    public SelectFunc(
        SExpr trueExpr,
        SExpr falseExpr
    ) {
        super(1, 1);
        _trueExpr  = trueExpr;
        _falseExpr = falseExpr;
    }

    protected Values _call(
        ContinuationStack stack,
        Environment       environment,
        Values            arguments
    ) {
        SExpr flag = arguments.at(0);

        stack.push(EvalFunc.INSTANCE);

        return new Values(
            (flag != SBool.FALSE)
            ? _trueExpr
            : _falseExpr
        );
    }
}


class AssignFunc extends Function
{
    private SSymbol _symbol;
    private boolean _isDefine;


    public AssignFunc(
        SSymbol symbol,
        boolean isDefine
    ) {
        super(1, 1);
        _symbol   = symbol;
        _isDefine = isDefine;
    }

    protected Values _call(
        ContinuationStack stack,
        Environment       environment,
        Values            arguments
    ) {
        SExpr value = arguments.at(0);



        return null;
    }
}


public class SyntaxFunc
    extends EnumeratedFunc
{
    // *** some constants ****************************************************

    public final static SyntaxFunc DEFINE_FUNC =
        new SyntaxFunc(
            DEFINE,
            "primitive-define",
            2, 2
        );

    public final static SyntaxFunc SET_FUNC =
        new SyntaxFunc(
            SET,
            "set!",
            2, 2
        );

    public final static SyntaxFunc QUOTE_FUNC =
        new SyntaxFunc(
            QUOTE,
            "quote",
            1, 1
        );

    public final static SyntaxFunc IF_FUNC =
        new SyntaxFunc(
            IF,
            "if",
            2, 3
        );

    public final static SyntaxFunc LAMBDA_FUNC =
        new SyntaxFunc(
            LAMBDA,
            "lambda",
            2, -1
        );

    public final static SyntaxFunc BEGIN_FUNC =
        new SyntaxFunc(
            BEGIN,
            "begin",
            1, -1
        );

    // *** constructors ******************************************************

    private SyntaxFunc(int id, String name, int minArity, int maxArity)
    {
        super(id, name, minArity, maxArity);
    }

    // *** Operator implementation *******************************************

    protected Values _call(
        ContinuationStack stack,
        Environment       environment,
        Values            arguments
    ) throws SException {
        switch(id()) {

        case BEGIN:
            {
                int i = arguments.getLength();

                stack.push(EvalFunc.INSTANCE);
                while (i > 0) {
                    stack.push(
                        new ConstFunc(
                            arguments.at(--i)
                        )
                    );
                    stack.push(EvalFunc.INSTANCE);
                }
                return new Values(
                    arguments.at(0 /* = i */)
                );
            }

        case LAMBDA:
            {
                int     minArity  = 0;
                boolean allowMore = false;
                Values  symbols;

                try {
                    // parse formals: a list with symbols,
                    // may be improper to allow optional
                    // arguments
                    ValuesFactory fab = new ValuesFactory();
                    SExpr formals = arguments.at(0);

                    // consume the proper list part
                    while (formals instanceof SPair) {
                        SPair   pair   = (SPair  )formals;
                        SSymbol symbol = (SSymbol)pair.getCar();

                        fab.append(symbol);
                        minArity++;

                        formals = pair.getCdr();
                    }

                    // the last cdr may be a symbol
                    if (formals != SEmpty.INSTANCE) {
                        SSymbol symbol = (SSymbol)formals;

                        fab.append(symbol);
                        allowMore = true;
                    }

                    symbols = fab.getValues();
                }
                catch (ClassCastException e) {
                    // a formal argument wasn't a symbol

                    throw new SImproperFormalsException(
                        arguments.at(0)
                    );
                }

                EnvironmentStub stub
                    = environment.newChildStub(symbols);

                if (stub.getSize() != symbols.getLength()) {
                    // some symbols occured more than once in
                    // the formal arguments

                    throw new SImproperFormalsException(
                        arguments.at(0)
                    );
                }

                Values  body = arguments.getTail();

                return new Values(
                    new ClosureFunc(
                        stub,
                        minArity,
                        allowMore,
                        body
                    )
                );
            }

        case QUOTE:
            return arguments;

        case DEFINE:
        case SET:
            try {
                SSymbol symbol = (SSymbol)arguments.at(0);
                SExpr   value  = arguments.at(1);

                stack.push(
                    new AssignFunc(
                        symbol,
                        (id() == DEFINE)
                    )
                );

                stack.push(
                    EvalFunc.INSTANCE
                );
                return new Values(value);
            }
            catch (ClassCastException e) {
                throw new SExpectedSymbolException(
                    arguments.at(0)
                );
            }


        case IF:
            {
                SExpr condition = arguments.at(0);
                SExpr trueCase  = arguments.at(1);
                SExpr falseCase =
                    (arguments.getLength() == 3)
                    ? arguments.at(3)
                    : null;

                stack.push(
                    new SelectFunc(
                        trueCase,
                        falseCase
                    )
                );
                stack.push(
                    EvalFunc.INSTANCE
                );
                return new Values(
                    condition
                );
            }

        default:
            throw new RuntimeException(
                "fatal: unknown SyntaxOperator " + id()
            );
        }
    }

    // ***********************************************************************
}
