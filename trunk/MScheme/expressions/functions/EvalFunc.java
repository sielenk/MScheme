package MScheme.expressions.functions;

import MScheme.expressions.SExpr;
import MScheme.expressions.SSymbol;
import MScheme.expressions.SList;
import MScheme.expressions.SPair;
import MScheme.expressions.SVector;

import MScheme.exceptions.SException;
import MScheme.exceptions.SExpectedListException;
import MScheme.exceptions.SCantEvaluateException;

import MScheme.machine.Values;
import MScheme.machine.ContinuationStack;

import MScheme.environment.Environment;

public class EvalFunc extends Function
{
    public final static Function INSTANCE = new EvalFunc();


    private EvalFunc()
    {
        super(1, 1);
    }


    protected Values _call(
        ContinuationStack stack,
        Environment       environment,
        Values            arguments
    ) throws SException {
        SExpr sexpr = arguments.at(0);

        if (sexpr instanceof SSymbol) {
            sexpr = environment.get((SSymbol)sexpr);
            // throws SSymbolNotFoundException
        } else if (sexpr instanceof SPair) {
            SPair pair = (SPair)sexpr;

            try {
                stack.push(
                    environment,
                    new ExpectFunctionFunc(
                        ((SList)(pair.getCdr())).toValues()
                    )
                );

                stack.push(
                    environment,
                    INSTANCE
                );
                sexpr = pair.getCar();
            } catch (ClassCastException e) {
                throw new SExpectedListException(pair.getCdr());
            }
        } else if (sexpr instanceof SVector) {
            throw new SCantEvaluateException(sexpr);
        }

        return new Values(sexpr);
    }


    protected String defaultString()
    {
        return "[eval]";
    }
}
