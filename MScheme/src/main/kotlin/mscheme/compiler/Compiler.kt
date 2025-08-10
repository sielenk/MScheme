/*
 * Created on 30.05.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.compiler

import mscheme.environment.StaticEnvironment
import mscheme.exceptions.CantCompileException
import mscheme.exceptions.CompileError
import mscheme.exceptions.SchemeException
import mscheme.syntax.ITranslator
import mscheme.syntax.ProcedureCall
import mscheme.values.ICompileable
import mscheme.values.ValueTraits


class Compiler(private val _env: StaticEnvironment) {
    @Throws(SchemeException::class, InterruptedException::class)
    fun getForceable(obj: Any?): Any? =
        if (ValueTraits.isScmVector(obj)) {
            throw CantCompileException(obj)
        } else if (ValueTraits.isSymbol(obj)) {
            _env.setStateClosed()
            _env.getDelayedReferenceFor(obj as String)
        } else if (obj is ICompileable) {
            obj.getForceable(_env)
        } else {
            _env.setStateClosed()
            ValueTraits.getConst(obj)
        }

    @Throws(SchemeException::class)
    fun getTranslator(obj: Any?): ITranslator {
        if (obj is String) {
            val result = _env.getSyntaxFor(obj)

            if (result != null) {
                return result
            }
        }

        return ProcedureCall.create(obj)
    }

    @Throws(SchemeException::class, InterruptedException::class)
    fun compile(compilee: Any?): Any? =
        force(getForceable(compilee))

    companion object {
        @JvmStatic
        @Throws(CompileError::class)
        fun force(o: Any?): Any? =
            if (o is IForceable) o.force() else o
    }
}