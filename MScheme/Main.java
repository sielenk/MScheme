package MScheme;

import MScheme.values.*;
import MScheme.environment.*;
import MScheme.code.*;
import MScheme.machine.Machine;

public class Main
{
    public final static void main(String[] argv)
        throws Exception
    {
        InputPort  in      = ValueFactory.createInputPort();
        OutputPort out     = ValueFactory.createOutputPort();
        Machine    machine = new Machine();
        
        out.write(machine.evaluate(in.read()));
        
        out.writeChar('\n');
    }
}

