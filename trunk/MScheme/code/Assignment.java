package MScheme.code;

import MScheme.machine.Machine;
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
    )
    {
        _binding          = binding;
        _valueCalculation = valueCalculation;
    }


    class AssignmentContinuation
        extends Continuation
    {
        AssignmentContinuation(Machine machine)
        { super(machine); }

        protected Code execute(Machine machine, Value evaluationResult)
        {
            machine.getEnvironment().assign(_binding, evaluationResult);
            return evaluationResult.getLiteral();
        }
    }

    public Code executionStep(Machine machine)
    {
        new AssignmentContinuation(machine);
        return _valueCalculation;
    }
}
