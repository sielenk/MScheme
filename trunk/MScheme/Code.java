package MScheme;

import MScheme.machine.Registers;

import MScheme.exceptions.SchemeException;


/**
 * The abstract base class for scheme machine instructions.
 * Opcode driven machines have to contain all the logic 
 * necessary to execute every single opcode. The scheme
 * machine implemented here only knows about this class.
 * The logic needed by the different instructions is contained
 * in the classes which implement them.
 */
public interface Code
{
    /** The CVS id of the file containing this class. */
    String id
        = "$Id$";


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
    Code executionStep(Registers state)
        throws SchemeException;
}
