package MScheme.code;

import MScheme.Value;
import MScheme.Code;

import MScheme.machine.Registers;
import MScheme.machine.Continuation;
import MScheme.environment.Reference;


public final class Assignment
    implements Code
{
    public final static String id
    = "$Id$";


    private final Reference _binding;
    private final Code      _valueCalculation;

    private Assignment(
        Reference binding,
        Code      valueCalculation
    )
    {
        _binding          = binding;
        _valueCalculation = valueCalculation;
    }

    public static Assignment create(
        Reference binding,
        Code      valueCalculation
    )
    {
        return new Assignment(binding, valueCalculation);
    }

    public Code executionStep(Registers state)
    {
        new Continuation(state)
        {
            public final static String id
            = "$Id$";


            protected Code execute(Registers regs, Value evaluationResult)
            {
                regs.getEnvironment().assign(_binding, evaluationResult);
                return evaluationResult.getLiteral();
            }


            protected String debugString()
            {
                return "assign[" + _binding + "]";
            }
        };

        return _valueCalculation;
    }


    public String toString()
    {
        return "SET[" + _binding + ' ' + _valueCalculation + ']';
    }
}
