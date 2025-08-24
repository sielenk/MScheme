/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine

import mscheme.exceptions.SchemeException


fun interface IContinuation {
    fun invoke(registers: Registers, value: Any?): Any?
}
