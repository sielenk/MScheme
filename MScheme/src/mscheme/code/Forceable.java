/*
 * Created on 03.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.code;

import mscheme.exceptions.CompileError;

/**
 * @author sielenk
 */
public interface Forceable
{
	/** The CVS id of the file containing this class. */
	String id
		= "$Id$";


	Reduceable force()
		throws CompileError;
}
