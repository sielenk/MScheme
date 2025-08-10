/*
 * Created on 03.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.compiler

import mscheme.exceptions.CompileError

/**
 * @author sielenk
 */
interface IForceable {
    @Throws(CompileError::class)
    fun force(): Any?
}
