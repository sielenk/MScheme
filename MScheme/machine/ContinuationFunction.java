/* The scheme value wrapper for continuations.
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

import java.io.IOException;
import java.io.Writer;

import MScheme.Code;
import MScheme.Value;

import MScheme.code.Sequence;

import MScheme.values.functions.UnaryFunction;

/**
 * Continuations are first class objects in scheme. They
 * seem to behave like unary functions. But if such a
 * function is called, continuations waiting to be invoked
 * are skipped.
 */
final class ContinuationFunction
    extends UnaryFunction
{
    /** The CVS id of the file containing this class. */
    public final static String id
        = "$Id$";


    private final Continuation _continuation;

    ContinuationFunction(Continuation continuation)
    {
        _continuation = continuation;
    }

    private static Code dynamicWind(
        Continuation source,
        Continuation destination,
        Value        result
    )
    {
        // this leave stack grown up, since first encountered frames are
        // left first
        final int      leaveStart = 0;
        int            leaveSp    = leaveStart;
        Code[]         leaveStack = new Code[Continuation.getLevel(source)];
        Continuation   from       = source;

        // This enter stack grows down, since first encountered frames are
        // entered last
        final int      enterStart = Continuation.getLevel(destination);
        int            enterSp    = enterStart;
        Code[]         enterStack = new Code[Continuation.getLevel(destination)];
        Continuation   to         = destination;

        while (from != to)
        {
            final int fromLevel = Continuation.getLevel(from);
            final int   toLevel = Continuation.getLevel(to  );

            if (fromLevel >= toLevel)
            {
                leaveSp = from.dynamicWindLeave(leaveStack, leaveSp);
                from    = from.getParent();
            }

            if (toLevel >= fromLevel)
            {
                enterSp = to.dynamicWindEnter(enterStack, enterSp);
                to      = to.getParent();
            }
        }

        final int numberOfLeaveThunks = leaveSp - leaveStart;
        final int numberOfEnterThunks = enterStart - enterSp;

        Code[] resultSequence = new Code[
            numberOfLeaveThunks + numberOfEnterThunks + 1
        ];

        System.arraycopy(
            leaveStack, 
            leaveStart, 
            resultSequence, 
            0, 
            numberOfLeaveThunks
        );

        System.arraycopy(
            enterStack,
            enterSp,
            resultSequence,
            numberOfLeaveThunks,
            numberOfEnterThunks
        );
        
        resultSequence[numberOfLeaveThunks + numberOfEnterThunks]
            = result;

        return Sequence.create(resultSequence);
    }


    // implementation of Value

    public void write(Writer destination)
    throws IOException
    {
        destination.write(
            "#[continuation]"
        );
    }

    public void display(Writer destination)
    throws IOException
    {
        destination.write(
            "#[continuation\n"
            + _continuation.toString()
            + "]"
        );
    }


    // implementation of UnaryFunction

    /**
     * 
     *
     * @returns  the code created during the stack unwinding.
     */
    protected Code checkedCall(Registers state, Value argument)
    {
        Continuation source      = state.getContinuation();
        Continuation destination = _continuation;

        state.setContinuation(destination);

        return dynamicWind(
            source,
            destination,
            argument
        );
    }
}
