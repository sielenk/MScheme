/*
 * Created on 15.02.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values;

import mscheme.code.Forceable;
import mscheme.environment.StaticEnvironment;
import mscheme.exceptions.SchemeException;

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public interface Compileable
{
	// compilation functions

	/**
	 * Compiles a value as normal code.
	 */
	Forceable getForceable(StaticEnvironment compilationEnv)
		throws SchemeException;
}
