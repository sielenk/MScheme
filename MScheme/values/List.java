package MScheme.values;

import MScheme.Value; 

import MScheme.environment.StaticEnvironment;
import MScheme.code.CodeList;

import MScheme.exceptions.*;


/**
 * This is an interface, because in scheme there is no list type.
 * Lists are composed of pairs and the empty list.
 * The classes representing those in this implementation are
 * at different levels in the inheritance hierarchy.
 */
public interface List
{
    /** The CVS id of the file containing this class. */
    String id
        = "$Id$";


    /**
     * Returns a reference to {@link Value}.
     */
    Value toValue();
    
    /**
     * Returns a reference to {@link Pair}.
     * <p>
     * @throws PairExpected if called on the empty list.
     */
    Pair toPair () throws PairExpected;
    
    /**
     * Returns <code>true</code> if called for the empty list,
     * <code>false</code> otherwise.
     */
    boolean isEmpty();

    /**
     *
     */
     int safeGetLength();

    /**
     *
     * @throws ListExpected if called on an improper list.
     */ 
    int getLength() throws ListExpected;

    /**
     *
     * @throws PairExpected if called on the empty list.
     */
    Value getHead() throws PairExpected;

    /**
     *
     */
    List  getTail() throws ListExpected;

    /**
     * 
     *
     * @throws ListExpected if called on an improper list.
     */
    List getReversed() throws ListExpected;

    /**
     *
     */
    CodeList getCodeList(StaticEnvironment e)
        throws CompileError, TypeError;
}
