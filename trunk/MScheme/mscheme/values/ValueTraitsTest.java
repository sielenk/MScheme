/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values;

import junit.framework.TestCase;

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class ValueTraitsTest extends TestCase
{
	/**
	 * Constructor for ValueTraitsTest.
	 * @param name
	 */
	public ValueTraitsTest(String name) {
		super(name);
	}

	final public void testIsTrue() {
		assertTrue (ValueTraits.isTrue(Boolean.TRUE ));
		assertFalse(ValueTraits.isTrue(Boolean.FALSE));
		
		assertTrue (ValueTraits.isTrue(ValueTraits.TRUE ));
		assertFalse(ValueTraits.isTrue(ValueTraits.FALSE));

		assertTrue(ValueTraits.isTrue(new Object()));
	}
}
