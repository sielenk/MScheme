/*
 * Created on 04.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values;

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public interface Comparable
{
	// scheme equivalence predicates (as in R5RS, 6.1)

	/**
	 * Compares two values for Scheme's <code>eq</code>-equality.
	 * This is the most discriminating equality in Scheme and is
	 * supposed to be easiest to check.
	 * <p>
	 * @param other the value with which to compare.
	 */
	boolean eq(Object other);

	/**
	 * Compares two values for Scheme's <code>eqv</code>-equality.
	 * This equality is supposed to compare characters and numbers
	 * of the same value as equal, even if they are not the same
	 * instance.
	 * <p>
	 * @param other the value with which to compare.
	 */
	boolean eqv(Object other);

	/**
	 * Compares two values for Scheme's <code>equal</code>-equality.
	 * This equality compares compound values recursively, primitive
	 * values like <code>eqv</code>.
	 * <p>
	 * @param other the value with which to compare.
	 */
	boolean equal(Object other);
}
