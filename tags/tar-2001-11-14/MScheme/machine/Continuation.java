/* The base class for all continuations.
   Copyright (C) 2001  Marvin H. Sielenkemper

This file is part of MScheme.

MScheme is free software; you can redistribute it and/or modify 
it under the terms of the GNU General Public License as published by 
the Free Software Foundation; either version 2 of the License, 
or (at your option) any later version. 

MScheme is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details. 

You should have received a copy of the GNU General Public License
along with MScheme; see the file COPYING. If not, write to 
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA  02111-1307, USA. */

package MScheme.machine;

import MScheme.Value;
import MScheme.Code;

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
    int dynamicWindLeave(Code[] sequence, int index)
    {
        return index;
    }

    /**
     *
     */
    int dynamicWindEnter(Code[] sequence, int index)
    {
        return index;
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
