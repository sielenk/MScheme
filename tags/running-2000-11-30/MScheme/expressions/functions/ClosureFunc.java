package MScheme.expressions.functions;


import MScheme.expressions.SExpr;
import MScheme.expressions.SBool;

import MScheme.machine.Values;
import MScheme.machine.ContinuationStack;

import MScheme.environment.Environment;
import MScheme.environment.EnvironmentStub;


public class ClosureFunc extends Function
{
    private EnvironmentStub _stub;
    private Values          _body;

    public ClosureFunc(
        EnvironmentStub stub,
        int             minArity,
        boolean         allowMore,
        Values          body
    ) {
        super(
            minArity,
            allowMore ? -1 : minArity
        );
        _stub = stub;
        _body = body;
    }

    protected Values _call(
        ContinuationStack stack,
        Environment       environment,
        Values            arguments
    ) {
        stack.push(
            _stub.fill(
                getMinArity(),
                (getMaxArity() == -1),
                arguments
            ),
            SyntaxFunc.BEGIN_FUNC
        );

        return _body;
    }


    protected String defaultString()
    {
        return "[closure]";
    }
}