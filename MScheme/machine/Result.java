package MScheme.machine;

import MScheme.Value;
import MScheme.Code;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


/**
 * This class is the abstract base for return instructions
 * of the scheme machine.
 */
public abstract class Result
    extends Code
{
    /** The CVS id of the file containing this class. */
    public final static String id
        = "$Id$";


    /** The default constructor. */
    protected Result()
    { }


    /**
     * Returns the result of an evaluation.
     * <p>
     * @param  state  the current state of the scheme machine.
     */
    protected abstract Value getValue(Registers state)
        throws RuntimeError;


    /**
     * Passes an evaluation result to the current continuation.
     * <p>
     * @param  state  the current state of the scheme machine.
     * @return the code returned by
     *         {@link Continuation#invoke(Registers, Value)}.
     */
    public final Code executionStep(Registers state)
        throws RuntimeError, TypeError
    {
        return state.getContinuation().invoke(
            state,
            getValue(state)
        );
    }
}
