/*
 * Created on 15.02.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.code;

import mscheme.exceptions.CompileError;
import mscheme.exceptions.SchemeException;
import mscheme.machine.Registers;
import mscheme.values.ValueTraits;

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class Literal
	implements Forceable, Reduceable
{
	/** The CVS id of the file containing this class. */
	public final static String id
		= "$Id$";

	private final Object _value;

	private Literal(Object value)
	{
		_value = value;
	}

	public static Literal create(Object value)
	{
		return new Literal(value);
	}

	public Reduceable force() throws CompileError
	{
		return this;
	}

    public Object reduce(Registers state) throws SchemeException
    {
        return ValueTraits.getCopy(_value);
    }
}
