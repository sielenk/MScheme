/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine

import mscheme.environment.DynamicEnvironment
import mscheme.environment.Reference
import mscheme.machine.stack.Stack

/**
 * @author sielenk
 */
class Registers internal constructor(var environment: DynamicEnvironment) {
    val stack: Stack = Stack()

    fun push(k: IContinuation) {
        stack.push(StackFrame(this.environment, k))
    }

    fun assign(key: Reference, value: Any?): Any? {
        return environment.assign(key, value)
    }
}
