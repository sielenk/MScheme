package MScheme.code;

import MScheme.machine.State;
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
        AssignmentContinuation(State state)
        { super(state); }

        protected Code execute(State state, Value evaluationResult)
        {
            state.getEnvironment().assign(_binding, evaluationResult);
            return evaluationResult.getLiteral();
        }
    }

    public Code executionStep(State state)
    {
        new AssignmentContinuation(state);
        return _valueCalculation;
    }
}
