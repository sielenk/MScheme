package MScheme;

import MScheme.expressions.*;
import MScheme.exceptions.SException;
import MScheme.environment.Environment;
import MScheme.machine.Machine;

public class Main
{
    public static void main(String argv[])
    {
        Environment env  = new Environment();
        SExpr       expr = new SPair(
            SEmpty.INSTANCE,
            SEmpty.INSTANCE
        );

        try {
            System.out.println(
                Machine.evaluate(env, expr)
            );
        }
        catch (SException e) {
            System.err.println(
                e.getSExpr()
                + " caused an "
                + e.getClass()
            );
        }
    }
}
