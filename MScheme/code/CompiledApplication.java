package MScheme.code;

import MScheme.machine.Machine;


final public class CompiledApplication
    extends Code
{
    final private CodeList _application;

    public CompiledApplication(CodeList application)
    { _application = application; }
    
    public Code executionStep(Machine machine)
    { return machine.handleApplication(_application); }
}

