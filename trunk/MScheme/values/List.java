package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value; 
import MScheme.Code;

import MScheme.environment.StaticEnvironment;
import MScheme.code.CodeList;

import MScheme.exceptions.*;


/**
 */
public abstract class List
    extends Compound
{
    /** The CVS id of the file containing this class. */
    String id
        = "$Id$";


    protected List(boolean isConst)
    { super(isConst); }


    // specialisation/implementation of Value
    
    private final void put(Writer destination, boolean doDisplay)
        throws IOException
    {
        destination.write('(');

        Value current = this;
        while (current instanceof Pair) {
            // 'this' is the first element of the list
            // and needs no leading space
            // (the opening parenthesis is a delimiter)
            if (current != this) {
                destination.write(' ');
            }

            Pair currentPair = (Pair)current;

            if (doDisplay) {
                currentPair.getFirst().display(destination);
            } else {
                currentPair.getFirst().write(destination);
            }

            current = currentPair.getSecond();
        }

        if (current != Empty.create()) {
            // 'this' is an improper list

            destination.write(" . ");

            if (doDisplay) {
                current.display(destination);
            } else {
                current.write(destination);
            }
        }

        destination.write(')');
    }

    public final void write(Writer destination)
        throws IOException
    { put(destination, false); }

    public final void display(Writer destination)
        throws IOException
    { put(destination, true); }


    // implementation of List

    public final int getLength()
        throws ListExpected
    {
        int result = safeGetLength();

        if (result < 0) {
            throw new ListExpected(this);
        } else {
            return result;
        }
    }

    public final List getReversed()
        throws ListExpected
    {
        List result = Empty.create();

        for (
            List rest = this;
            !rest.isEmpty();
            rest = rest.getTail()
        ) {
            result = ListFactory.prepend(
                rest.getHead(),
                result
            );
        }

        return result;
    }    

    public final List toList()
        throws ListExpected
    {
        if (isList()) {
            return this; 
        } else {
            throw new ListExpected(this);
        }
    }


    // abstract interface of list

    public abstract boolean isEmpty();
    public abstract boolean isList();

    public abstract int safeGetLength();

    public abstract Value getHead() throws PairExpected;
    public abstract List  getTail() throws ListExpected;

    public abstract Code getCode(StaticEnvironment env)
            throws CompileError, TypeError;

    public abstract CodeList getCodeList(StaticEnvironment compilationEnv)
        throws CompileError, TypeError;
}
