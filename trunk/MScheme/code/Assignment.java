package MScheme.code;

import MScheme.machine.Registers;
import MScheme.machine.Continuation;
import MScheme.environment.Reference;
import MScheme.values.Value;


final public class Assignment
    extends Code
{
    final private Reference _binding;
    final private Code      _valueCalculation;

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
            protected Code execute(Registers regs, Value evaluationResult)
            {
                regs.getEnvironment().assign(_binding, evaluationResult);
                return evaluationResult.getLiteral();
            }
        };

        return _valueCalculation;
    }
}
