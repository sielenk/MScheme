package MScheme.values;

import MScheme.environment.StaticEnvironment;
import MScheme.values.Value; 
import MScheme.code.CodeList;

import MScheme.exceptions.*;


public interface List
{
    Value toValue();
    Pair  toPair () throws PairExpected;
    
    boolean isEmpty();
    
    int safeGetLength();
    int     getLength() throws ListExpected;

    Value getHead() throws PairExpected;
    List  getTail() throws ListExpected;

    List getReversed() throws ListExpected;

    CodeList getCodeList(StaticEnvironment e)
        throws CompileError, TypeError;
}
