package MScheme;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value; 
import MScheme.Code;

import MScheme.environment.StaticEnvironment;
import MScheme.code.CodeList;

import MScheme.exceptions.*;


public interface List
    extends Value
{
    /** The CVS id of the file containing this class. */
    String id
        = "$Id$";


    boolean isEmpty();

    int   getLength  () throws ListExpected;
    List  getReversed() throws ListExpected;
    Value getHead    () throws PairExpected;
    List  getTail    () throws ListExpected;

    Code getCode(StaticEnvironment env)
        throws CompileError, TypeError;

    CodeList getCodeList(StaticEnvironment compilationEnv)
        throws CompileError, TypeError;
}
