/*
 * Created on 15.02.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.environment;

import mscheme.code.Forceable;
import mscheme.code.Reduceable;
import mscheme.exceptions.CompileError;

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public final class DelayedReference
	implements Forceable
{
	public final static String id
		= "$Id$";


	private final String _symbol;
	private final StaticEnvironment _env;
	private final boolean           _restricted;


	DelayedReference(
		String            key,
		StaticEnvironment env,
		boolean           restricted
	)
	{
		_symbol     = key;
		_env        = env;
		_restricted = restricted;
	}


	public Reference forceRef()
	throws CompileError
	{
		return _env.getReferenceFor(_symbol, _restricted);
	}


    public Reduceable force()
    throws CompileError
    {
        return forceRef();
    }
}
