package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;

import MScheme.environment.StaticEnvironment;
import MScheme.code.CodeList;

import MScheme.exceptions.ListExpected;
import MScheme.exceptions.PairExpected;
import MScheme.exceptions.SchemeException;


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
