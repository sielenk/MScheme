package MScheme;

import MScheme.code.CodeList;
import MScheme.code.Application;

import MScheme.machine.Registers;

import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.SchemeException;


/**
 * The abstract base class for scheme machine instructions.
 * Opcode driven machines have to contain all the logic 
 * necessary to execute every single opcode. The scheme
 * machine implemented here only knows about this class.
 * The logic needed by the different instructions is contained
 * in the classes which implement them.
 */
public abstract class Code
            implements Translator
{
    /** The CVS id of the file containing this class. */
    public final static String id
    = "$Id$";


    /** The default constructor. */
    protected Code()
    { }


    /**
     * Executes some calculation.
     * Derived classes have to implement this function to
     * provide appropriate actions.
     * Such actions will be either
     *   returning a {@link MScheme.machine.Result}
     * or
     *   pushing a {@link MScheme.machine.Continuation}
     *   and returning code for a sub-calculation.
     * <p>
     * @param state the current state of the scheme machine.
     *
     * @return the next instruction to execute.
     */
    public abstract Code executionStep(Registers state)
    throws SchemeException;


    /**
     * The compilation function for lists without a syntactic keyword
     * as first element.
     * Such lists compile to an application. The result of the evaluation
     * of <code>this</code> will be the function to be called.
     * <p>
     * @param compilationEnv the current static environment.
     * @param arguments      the tail of the list, its elements will be
     *                       evaluated to the functions arguments.
     *
     * @return an instance of {@link MScheme.code.Application Application}.
     */
    public final Code translate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        return Application.create(
                   CodeList.prepend(
                       this,
                       arguments.getCodeList(compilationEnv)
                   )
               );
    }


    public abstract String toString();
}
