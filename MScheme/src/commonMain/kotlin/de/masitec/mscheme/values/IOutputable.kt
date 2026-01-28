/*
 * Created on 04.01.2004
 */
package de.masitec.mscheme.values

import de.masitec.mscheme.util.Writer


/**
 * @author sielenk
 */
interface IOutputable {
    /**
     * Part of Scheme's `write` function implementation.
     *
     * @param destination the java writer which expects the output.
     */
    fun outputOn(destination: Writer, doWrite: Boolean)
}
