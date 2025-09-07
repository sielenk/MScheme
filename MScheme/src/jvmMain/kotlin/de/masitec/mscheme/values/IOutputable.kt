/*
 * Created on 04.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package de.masitec.mscheme.values

import java.io.Writer

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
