package MScheme.machine;

import MScheme.Value;
import MScheme.Code;

import MScheme.code.CodeList;

import MScheme.exceptions.SchemeException;


/**
 * The base class for all continuations.
 */
public abstract class Continuation
    extends    Registers
    implements Cloneable
{
    /** The CVS id of the file containing this class. */
    public final static String id
    = "$Id$";


    private       int       _level;


    /**
     * Initializes a new continuation and pushes it on the stack.
     * Since it is pushed here, the result of the creation
     * by calling <code>new</code> is usually ignored.
     * <p>
     * @param state  the current state of the scheme machine.
     */
    protected Continuation(Registers state)
    {
        super(state);
        _level = getLevel(getParent()) + 1;

        state.setContinuation(this);
    }


    Continuation cloneContinuation()
    {
        try {
            return (Continuation)clone();
        }
        catch (CloneNotSupportedException e)
        {
            throw new RuntimeException(
                "unexpected CloneNotSupportedException"
            );
        }
    }

    static Continuation copyAndPrependSubcontinuation(
        Continuation oldRootsParent,
        Continuation oldLeaf,
        Continuation newRootsParent
    )
    {
        if (oldLeaf == oldRootsParent)
        {
            return newRootsParent;
        }

        Continuation newLeaf = oldLeaf.cloneContinuation();

        newLeaf._level -= getLevel(oldRootsParent);
        newLeaf._level += getLevel(newRootsParent);

        newLeaf.setContinuation(
            copyAndPrependSubcontinuation(
                oldRootsParent,
                oldLeaf.getParent(),
                newRootsParent
            )
        );

        return newLeaf;
    }


    /**
     * Returns the content of the captured continuation register.
     */
    final Continuation getParent()
    {
        return getContinuation();
    }

    /**
     * Returns the depth of a continuation in the stack.
     */
    final static int getLevel(Continuation c)
    {
        return (c == null) ? -1 : c._level;
    }

    /**
     *
     */
    CodeList dynamicWindLeave(CodeList sequence)
    {
        return sequence;
    }

    /**
     *
     */
    CodeList dynamicWindEnter(CodeList sequence)
    {
        return sequence;
    }


    /**
     * Restores the captured state and calls {@link #execute}.
     * The parameters are just passed on.
     * <p>
     * @return  the result of the call to {@link #execute}.
     */
    final Code invoke(Registers state, Value result)
    throws SchemeException
    {
        Registers buffer = new Registers(this);
        Code      next   = execute(buffer, result);
        state.assign(buffer);
        return next;
    }

    /**
     * Implements the concrete behaviour of the continuation.
     */
    protected abstract Code execute(Registers state, Value result)
    throws SchemeException;


    protected abstract String debugString();

    public final String toString()
    {
        StringBuffer buffer = new StringBuffer();

        for (
            Continuation current = this;
            current != null;
            current = current.getParent()
        )
        {
            buffer.append(getLevel(current));
            buffer.append(" : ");
            buffer.append(current.debugString());
            buffer.append('\n');
        }

        return buffer.toString();
    }
}
