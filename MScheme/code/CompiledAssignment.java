package MScheme.code;

import MScheme.machine.Machine;
import MScheme.environment.Reference;


final public class CompiledAssignment
    extends Code
{
    final private Reference _binding;
    final private Code      _valueCalculation;

    public CompiledAssignment(
        Reference binding,
        Code      valueCalculation
    )
    {
        _binding          = binding;
        _valueCalculation = valueCalculation;
    }
    
    public Code executionStep(Machine machine)
    {
        machine.storeValueAt(_binding);
        return _valueCalculation;
    }
}
