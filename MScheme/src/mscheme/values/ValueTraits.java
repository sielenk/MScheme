/*
 * Created on 02.01.2004
 *
 */
package mscheme.values;

import java.io.IOException;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import mscheme.environment.Environment;
import mscheme.environment.StaticEnvironment;
import mscheme.exceptions.CantCompileException;
import mscheme.exceptions.CharExpected;
import mscheme.exceptions.EnvironmentExpected;
import mscheme.exceptions.FunctionExpected;
import mscheme.exceptions.InputPortExpected;
import mscheme.exceptions.ListExpected;
import mscheme.exceptions.NumberExpected;
import mscheme.exceptions.OutputPortExpected;
import mscheme.exceptions.PairExpected;
import mscheme.exceptions.RuntimeError;
import mscheme.exceptions.SchemeException;
import mscheme.exceptions.StringExpected;
import mscheme.exceptions.SymbolExpected;
import mscheme.exceptions.VectorExpected;
import mscheme.machine.Registers;
import mscheme.syntax.ProcedureCall;
import mscheme.syntax.ITranslator;


/**
 * @author sielenk
 * 
 */
public class ValueTraits
{
    public final static Boolean TRUE  = Boolean.TRUE;
    public final static Boolean FALSE = Boolean.FALSE;

    public static boolean isTrue(Object o)
    {
        return (o != Boolean.FALSE);
    }

    public static boolean isEmpty(Object object)
    {
        return object == ListFactory.create();
    }

    public static Object apply(Registers state, Object function, IList arguments)
        throws SchemeException
    {
        if (function instanceof Method)
        {
            Method m = (Method)function;

            try
            {
                if (arguments.isEmpty())
                {
                    return m.invoke(null, null);
                }
                else
                {
                    return m.invoke(arguments.getHead(), arguments.getTail()
                            .getArray());
                }
            }
            catch (IllegalArgumentException e1)
            {
                throw new RuntimeError(function, e1.toString());
            }
            catch (IllegalAccessException e1)
            {
                throw new RuntimeError(function, e1.toString());
            }
            catch (InvocationTargetException e1)
            {
                throw new RuntimeError(function, e1.toString());
            }
        }
        else if (function instanceof Function)
        {
            return ((Function)function).call(state, arguments);
        }
        else
        {
            throw new FunctionExpected(function);
        }
    }

    public static boolean eq(Object fst, Object snd)
    {
        return (fst instanceof IComparable)
        	? ((IComparable)fst).eq(snd)
            : (fst == snd);
    }

    public static boolean eqv(Object fst, Object snd)
    {
        if (fst instanceof Character)
        {
            return fst.equals(snd);
        }
        else
        {
            return (fst instanceof IComparable)
            	? ((IComparable)fst).eqv(snd)
                : (fst == snd);
        }
    }

    public static boolean equal(Object fst, Object snd)
    {
        return (fst instanceof IComparable)
        	? ((IComparable)fst).equals(snd)
        	: fst.equals(snd);
    }

    public static boolean isList(Object o)
    {
        return (o instanceof IList) && ((IList)o).isValid();
    }

    public static IList toList(Object o) throws ListExpected
    {
        if (o instanceof IList)
        {
            return ((IList)o).validate();
        }
        else
        {
            throw new ListExpected(o);
        }
    }

    public static boolean isPair(Object o)
    {
        return o instanceof IPair;
    }

    public static IPair toPair(Object o) throws PairExpected
    {
        if (o instanceof IPair)
        {
            return (IPair)o;
        }
        else
        {
            throw new PairExpected(o);
        }
    }

    public static InputPort toInputPort(Object o) throws InputPortExpected
    {
        if (o instanceof InputPort)
        {
            return (InputPort)o;
        }
        else
        {
            throw new InputPortExpected(o);
        }
    }

    public static Symbol toSymbol(Object o) throws SymbolExpected
    {
        if (o instanceof Symbol)
        {
            return (Symbol)o;
        }
        else
        {
            throw new SymbolExpected(o);
        }
    }

    public static Boolean toScmBoolean(boolean b)
    {
        return Boolean.valueOf(b);
    }

    public static Boolean toScmBoolean(Object o)
    {
        return Boolean.valueOf(isTrue(o));
    }

    public static ScmNumber toScmNumber(Object o) throws NumberExpected
    {
        if (o instanceof ScmNumber)
        {
            return (ScmNumber)o;
        }
        else
        {
            throw new NumberExpected(o);
        }
    }

    public static Object toScmNumber(int i)
    {
        return ScmNumber.create(i);
    }

    public static Character toScmChar(Object o) throws CharExpected
    {
        if (o instanceof Character)
        {
            return (Character)o;
        }
        else
        {
            throw new CharExpected(o);
        }
    }

    public static Character toScmChar(char c)
    {
        return new Character(c);
    }

    public static ScmString toScmString(Object o) throws StringExpected
    {
        if (o instanceof ScmString)
        {
            return (ScmString)o;
        }
        else
        {
            throw new StringExpected(o);
        }
    }

    public static ScmVector toScmVector(Object o) throws VectorExpected
    {
        if (o instanceof ScmVector)
        {
            return (ScmVector)o;
        }
        else
        {
            throw new VectorExpected(o);
        }
    }

    public static OutputPort toOutputPort(Object o) throws OutputPortExpected
    {
        if (o instanceof OutputPort)
        {
            return (OutputPort)o;
        }
        else
        {
            throw new OutputPortExpected(o);
        }
    }

    public static Environment toEnvironment(Object o)
        throws EnvironmentExpected
    {
        if (o instanceof Environment)
        {
            return (Environment)o;
        }
        else
        {
            throw new EnvironmentExpected(o);
        }
    }

    public static StaticEnvironment toStaticEnvironment(Object o)
        throws EnvironmentExpected
    {
        if (o instanceof StaticEnvironment)
        {
            return (StaticEnvironment)o;
        }
        else
        {
            throw new EnvironmentExpected(o);
        }
    }

    public static boolean isScmBoolean(Object o)
    {
        return o instanceof Boolean;
    }

    public static boolean isSymbol(Object o)
    {
        return o instanceof Symbol;
    }

    public static boolean isScmNumber(Object o)
    {
        return o instanceof ScmNumber;
    }

    public static boolean isScmChar(Object o)
    {
        return o instanceof Character;
    }

    public static boolean isScmString(Object o)
    {
        return o instanceof ScmString;
    }

    public static boolean isScmVector(Object o)
    {
        return o instanceof ScmVector;
    }

    public static boolean isPort(Object o)
    {
        return o instanceof Port;
    }

    public static boolean isFunction(Object o)
    {
        return o instanceof Function;
    }

    public static void output(Writer destination, boolean doWrite, Object o)
        throws IOException
    {
        if (o instanceof Character)
        {
            final char c = ((Character)o).charValue();

            if (doWrite)
            {
                destination.write("#\\");
                switch (c)
                {
                case ' ':
                    destination.write("space");
                    break;

                case '\n':
                    destination.write("newline");
                    break;

                default:
                    destination.write(c);
                    break;
                }
            }
            else
            {
                destination.write(c);
            }
        }
        else if (isScmBoolean(o))
        {
            destination.write(isTrue(o) ? "#t" : "#f");
        }
        else if (o instanceof IOutputable)
        {
            ((IOutputable)o).outputOn(destination, doWrite);
        }
        else
        {
            if (doWrite)
            {
                destination.write("#[" + o.toString() + "]");
            }
            else
            {
                destination.write(o.toString());
            }
        }
    }

    public static void display(Writer destination, Object o) throws IOException
    {
        output(destination, false, o);
    }

    public static void write(Writer destination, Object o) throws IOException
    {
        output(destination, true, o);
    }

    public static Object getConst(Object o)
    {
        if (o instanceof IMutable)
        {
            return ((IMutable)o).getConst();
        }
        else
        {
            return o;
        }
    }

    public static Object getCompiled(StaticEnvironment compilationEnv,
        Object object) throws SchemeException
    {
        if (isScmVector(object))
        {
            throw new CantCompileException(object);
        }
        else if (object instanceof ICompileable)
        {
            return ((ICompileable)object).getCompiled(compilationEnv);
        }
        else
        {
            compilationEnv.setStateClosed();
            return getConst(object);
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
}