/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.code;

import mscheme.exceptions.SchemeException;
import mscheme.machine.Registers;

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public interface Reduceable
{
	public final static String id
		= "$Id$";

	Object reduce(Registers state) throws SchemeException;
}
