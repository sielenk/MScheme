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
    public static Object force(Object o)
        throws CompileError
    {
    	return
    		(o instanceof IForceable)
    		? ((IForceable)o).force()
    		: o;
    }

    public static Object getForceable(StaticEnvironment compilationEnv,
        Object object) throws SchemeException
    {
        if (ValueTraits.isScmVector(object))
        {
            throw new CantCompileException(object);
        }
        else if (object instanceof ICompileable)
        {
            return ((ICompileable)object).getForceable(compilationEnv);
        }
        else
        {
            compilationEnv.setStateClosed();
            return ValueTraits.getConst(object);
        }
    }

    public static ITranslator getTranslator(StaticEnvironment compilationEnv,
        Object object) throws SchemeException
    {
        if (object instanceof Symbol)
        {
            Symbol symbol = (Symbol)object;
            
            ITranslator result = compilationEnv.getSyntaxFor(symbol);
            
            if (result != null)
            {
                return result;
            }
        }
    
        return ProcedureCall.create(object);
    }

    public static Object compile(
        StaticEnvironment compilationEnv,
        Object            compilee
    ) throws SchemeException
    {
        return
        	force(
            	getForceable(compilationEnv, compilee));
    }
}
