package MScheme;

import MScheme.values.*;
import MScheme.environment.*;
import MScheme.code.*;
import MScheme.machine.Machine;

public class Main
{
    public final static String id = "$Id$";

    public final static void main(String[] argv)
        throws Exception
    {
        Machine machine = new Machine();

        for (int i = 0; i < argv.length; i++) {
            InputPort in = InputPort.create(argv[i]);

            System.out.println("parsing " + argv[i]);

            Value v;
            while ((v = in.read()) != in.EOF_VALUE) {
                machine.evaluate(v);
            }
        }
    }
}
