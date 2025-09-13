/*
 * Created on 04.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package de.masitec.mscheme.values

interface IComparable {
    // scheme equivalence predicates (as in R5RS, 6.1)

    /**
     * Compares two values for Scheme's `eq`-equality. This is the most discriminating
     * equality in Scheme and is supposed to be easiest to check.
     *
     * @param other the value with which to compare.
     */
    fun eq(other: Any?): Boolean

    /**
     * Compares two values for Scheme's `eqv`-equality. This equality is supposed to
     * compare characters and numbers of the same value as equal, even if they are not the same
     * instance.
     *
     * @param other the value with which to compare.
     */
    fun eqv(other: Any?): Boolean

    /**
     * Compares two values for Scheme's `equal`-equality. This equality compares compound
     * values recursively, primitive values like `eqv`.
     *
     * @param other the value with which to compare.
     */
    override fun equals(other: Any?): Boolean
}
