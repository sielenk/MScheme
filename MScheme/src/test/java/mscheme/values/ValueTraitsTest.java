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
public class ValueTraitsTest extends TestCase {

  /**
   * Constructor for ValueTraitsTest.
   */
  public ValueTraitsTest(String name) {
    super(name);
  }

  final public void testIsTrue() {
    assertTrue(ValueTraits.INSTANCE.isTrue(Boolean.TRUE));
    assertFalse(ValueTraits.INSTANCE.isTrue(Boolean.FALSE));

    assertTrue(ValueTraits.INSTANCE.isTrue(ValueTraits.TRUE));
    assertFalse(ValueTraits.INSTANCE.isTrue(ValueTraits.FALSE));

    assertTrue(ValueTraits.INSTANCE.isTrue(new Object()));
  }
}
