package MScheme;

import java.io.Writer;
import java.io.IOException;

import MScheme.environment.StaticEnvironment;
import MScheme.code.CodeList;

import MScheme.exceptions.*;


public interface List
    extends Value
{
    /** The CVS id of the file containing this class. */
    String id
    = "$Id$";


    int   getLength  () throws ListExpected;
    List  getReversed() throws ListExpected;
    Value getHead    () throws PairExpected;
    List  getTail    () throws PairExpected, ListExpected;

    CodeList getCodeList(StaticEnvironment compilationEnv)
    throws SchemeException;
}