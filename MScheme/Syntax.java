package MScheme;

import MScheme.values.List;

import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.SchemeException;


/**
 * This interface is used to compile lists. Due to the special
 * nature of syntactic keywords in Scheme - they are not reserved -
 * their bindings are stored in the static environment.
 */
public interface Syntax
{
    /** The CVS id of the file containing this class. */
    String id
        = "$Id$";


    Code translate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException;
}
