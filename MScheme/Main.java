package MScheme;

import MScheme.expressions.*;
import MScheme.exceptions.SException;
import MScheme.environment.Environment;
import MScheme.environment.EnvironmentFactory;
import MScheme.machine.Machine;

public class Main
{
    public static void main(String argv[])
    {
        Environment env  = EnvironmentFactory.getEmpty();
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
                + e.getClass().getName()
            );
        }
    }
}
