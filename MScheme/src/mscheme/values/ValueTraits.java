/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import mscheme.Syntax;
import mscheme.code.Forceable;
import mscheme.code.Literal;
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

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class ValueTraits
{
	public final static Boolean TRUE  = Boolean.TRUE;
	public final static Boolean FALSE = Boolean.FALSE;

	/**
	 * @param value
	 * @return
	 */
	public static boolean isTrue(Object o)
	{
		return (o != Boolean.FALSE);
	}

	/**
	 * @param object
	 * @return
	 */
	public static boolean isEmptyList(Object object)
	{
		return object == Empty.create();
	}

	public static Object apply(
		Registers state,
		Object    function,
		List      arguments)
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
				else if (Modifier.isStatic(m.getModifiers()))
				{
					return m.invoke(
						null,
						arguments.getArray());
				}
				else
				{
					return m.invoke(
						arguments.getHead(),
						arguments.getTail().getArray());
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
		return
			(fst instanceof Comparable)
			? ((Comparable)fst).eq(snd)
			: (fst == snd);
	}

	public static boolean eqv(Object fst, Object snd)
	{
		return
			(fst instanceof Comparable)
			? ((Comparable)fst).eqv(snd)
			: fst.equals(snd);
	}

	public static boolean equal(Object fst, Object snd)
	{
		return
			(fst instanceof Comparable)
			? ((Comparable)fst).equal(snd)
			: isScmString(fst)
			  ? ScmString.equals((char[])fst, snd)
			  : isScmVector(fst)
			    ? ScmVector.equals((Object[])fst, snd)
			    : fst.equals(snd);
	}


	public static boolean isList(Object o)
	{
		return (o instanceof List) && ((List)o).isValid();
	}

	public static List toList(Object o)
	throws ListExpected
	{
		if (isList(o))
		{
			return (List)o;
		}
		else
		{
			throw new ListExpected(o);
		}
	}

	public static Pair toPair(Object o)
	throws PairExpected
	{
		try	{ return (Pair)o; }
		catch (ClassCastException e) { throw new PairExpected(o); }
	}

	public static InputPort toInputPort(Object o)
	throws InputPortExpected
	{
		try	{ return (InputPort)o; }
		catch (ClassCastException e) { throw new InputPortExpected(o); }
	}

	public static String toSymbol(Object o)
	throws SymbolExpected
	{
		try	{ return (String)o; }
		catch (ClassCastException e) { throw new SymbolExpected(o); }
	}

	public static Boolean toScmBoolean(boolean b)	
	{
		return Boolean.valueOf(b);
	}

	public static Boolean toScmBoolean(Object o)
	{
		return Boolean.valueOf(isTrue(o));
	}

	public static ScmNumber toScmNumber(Object o)
	throws NumberExpected
	{
		try	{ return (ScmNumber)o; }
		catch (ClassCastException e) { throw new NumberExpected(o); }
	}

	public static Object toScmNumber(int i)
	{
		return ScmNumber.create(i);
	}

	public static Character toScmChar(Object o)
	throws CharExpected
	{
		try	{ return (Character)o; }
		catch (ClassCastException e) { throw new CharExpected(o); }
	}

	public static Character toScmChar(char c)
	{
		return new Character(c);
	}

	public static char[] toScmString(Object o)
	throws StringExpected
	{
		try { return (char[])o; }
		catch (ClassCastException e) { throw new StringExpected(o); }
	}

	public static Object[] toScmVector(Object o)
	throws VectorExpected
	{
		try	{ return (Object[])o; }
		catch (ClassCastException e) { throw new VectorExpected(o); }
	}

	public static OutputPort toOutputPort(Object o)
	throws OutputPortExpected
	{
		try	{ return (OutputPort)o; }
		catch (ClassCastException e) { throw new OutputPortExpected(o); }
	}

	public static Environment toEnvironment(Object o)
	throws EnvironmentExpected
	{
		try	{ return (Environment)o; }
		catch (ClassCastException e) { throw new EnvironmentExpected(o); }
	}

	public static StaticEnvironment toStaticEnvironment(Object o)
	throws EnvironmentExpected
	{
		try	{ return (StaticEnvironment)o; }
		catch (ClassCastException e) { throw new EnvironmentExpected(o); }
	}

	public static boolean isScmBoolean(Object o)
	{
		return o instanceof Boolean;
	}

	public static boolean isPair(Object o)
	{
		return o instanceof Pair;
	}

	public static boolean isSymbol(Object o)
	{
		return o instanceof String;
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
		return o instanceof char[];
	}

	public static boolean isScmVector(Object o)
	{
		return o instanceof Object[];
	}

	public static boolean isPort(Object o)
	{
		return (o instanceof InputPort) || (o instanceof OutputPort);
	}

	public static boolean isFunction(Object o)
	{
		return (o instanceof Function) || (o instanceof Method) ;
	}


	public static void output(Object o, Writer destination, boolean doWrite)
	    throws IOException, SchemeException
	{
		if (isScmBoolean(o))
		{
			destination.write(
				isTrue(o)
				? "#t"
				: "#f");
		}
		else if (isScmChar(o))
		{
            ScmChar.outputOn(
            	toScmChar(o),
            	destination,
            	doWrite);
		}
		else if (isScmVector(o))
		{
			ScmVector.outputOn(
				toScmVector(o),
				destination,
				doWrite);
		}
		else if (isScmString(o))
		{
			ScmString.outputOn(
				toScmString(o),
				destination,
				doWrite);
		}
		else if (isSymbol(o))
		{
			destination.write(
				o.toString());
		}
		else if (o instanceof Outputable)
		{
			((Outputable)o).outputOn(destination, doWrite);
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

	public static void display(Object o, Writer destination)
	throws IOException, SchemeException
	{
		output(o, destination, false);
	}

	public static void write(Object o, Writer destination)
	throws IOException, SchemeException
	{
		output(o, destination, true);
	}

	public static String toString(Object value)
	{
		StringWriter buffer = new StringWriter();

		try
        { write(value, buffer); }
        catch (IOException e)
        { }
        catch (SchemeException e)
        { }

		return buffer.toString();
	}


	public static Forceable getForceable(
		StaticEnvironment compilationEnv,
		Object            object)
	throws SchemeException
	{
		if (isSymbol(object))
		{
			String symbol = toSymbol(object);

			compilationEnv.setStateClosed();
			return compilationEnv.getDelayedReferenceFor(symbol);
		}
		else if (isScmVector(object))
		{
			throw new CantCompileException(object);
		}
		else if (object instanceof Compileable)
		{
			return ((Compileable)object).getForceable(compilationEnv);
		}
		else
		{
			compilationEnv.setStateClosed();

			return Literal.create(object); 
		}
	}

	public static Syntax getSyntax(
		StaticEnvironment compilationEnv,
		Object            object)
	throws SchemeException
	{
		if (isSymbol(object))
		{
			String symbol = toSymbol(object);
			Syntax result = compilationEnv.getSyntaxFor(symbol);
            
            if (result != null)
            {
	            return result;
            }
		}

		return ProcedureCall.create(object);
	}


	private static int i = 0;

    public static String createUniqueSymbol()
    {
        return "#[id " + (i++) + "]";
    }

    public static Object getCopy(Object value) throws StringExpected, ListExpected, VectorExpected
    {
    	if (isScmString(value))
    	{
			return ScmString.copy(toScmString(value));
    	}
    	else if (isScmVector(value))
    	{
    		return ScmVector.copy(toScmVector(value));
    	}
    	else if (isList(value))
    	{
    		return toList(value).getCopy();
    	}
    	else
    	{
	        return value;
		}
    }
}
