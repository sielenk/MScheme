package MScheme.code;

import MScheme.Value;
import MScheme.Code;

import MScheme.machine.Registers;
import MScheme.machine.Continuation;
import MScheme.environment.Reference;


public final class Assignment
    extends Code
{
    public final static String id
        = "$Id$";

    private final Reference _binding;
    private final Code      _valueCalculation;

    public Assignment(
        Reference binding,
        Code      valueCalculation
    ) {
        _binding          = binding;
        _valueCalculation = valueCalculation;
    }


    public Code executionStep(Registers registers)
    {
        new Continuation(registers) {
            public final static String id
                = "$Id$";

            protected Code execute(Registers regs, Value evaluationResult)
            {
                regs.getEnvironment().assign(_binding, evaluationResult);
                return evaluationResult.getLiteral();
            }
        };

        return _valueCalculation;
    }
}
