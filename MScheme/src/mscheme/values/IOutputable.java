/*
 * Created on 04.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values;

import java.io.IOException;
import java.io.Writer;

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public interface IOutputable
{
	/**
	 * Part of Scheme's <code>write</code> function implementation.
	 * <p>
	 * @param  destination  the java writer which expects the output.
	 */
	void outputOn(Writer destination, boolean doWrite) throws IOException;
}
