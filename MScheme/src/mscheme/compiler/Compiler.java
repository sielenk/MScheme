/*
 * Created on 30.05.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.compiler;

import mscheme.environment.StaticEnvironment;
import mscheme.exceptions.CantCompileException;
import mscheme.exceptions.CompileError;
import mscheme.exceptions.SchemeException;
import mscheme.syntax.ITranslator;
import mscheme.syntax.ProcedureCall;
import mscheme.values.ICompileable;
import mscheme.values.Symbol;
import mscheme.values.ValueTraits;

/**
 * @author sielenk
 * 
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class Compiler
{
    private final StaticEnvironment _env;

    public Compiler(StaticEnvironment env)
    {
        _env = env;
    }

    public static Object force(Object o)
            throws CompileError
    {
        return (o instanceof IForceable) ? ((IForceable) o).force() : o;
    }

    public Object getForceable(Object object)
            throws SchemeException, InterruptedException
    {
        if (ValueTraits.isScmVector(object))
        {
            throw new CantCompileException(object);
        }
        else if (object instanceof ICompileable)
        {
            return ((ICompileable) object).getForceable(_env);
        }
        else
        {
            _env.setStateClosed();
            return ValueTraits.getConst(object);
        }
    }

    public ITranslator getTranslator(Object object)
            throws SchemeException
    {
        if (object instanceof Symbol)
        {
            Symbol symbol = (Symbol) object;

            ITranslator result = _env.getSyntaxFor(symbol);

            if (result != null)
            {
                return result;
            }
        }

        return ProcedureCall.create(object);
    }

    public Object compile(Object compilee)
            throws SchemeException, InterruptedException
    {
        return force(getForceable(compilee));
    }
}