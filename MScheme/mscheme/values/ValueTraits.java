/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values;

import java.io.IOException;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import mscheme.Syntax;
import mscheme.Value;
import mscheme.environment.Environment;
import mscheme.environment.StaticEnvironment;
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
			((fst instanceof Value) && (snd instanceof Value))
			? ((Value)fst).eq((Value)snd)
			: (fst == snd);
	}

	public static boolean eqv(Object fst, Object snd)
	{
		return
			((fst instanceof Value) && (snd instanceof Value))
			? ((Value)fst).eqv((Value)snd)
			: fst.equals(snd);
	}

	public static boolean equal(Object fst, Object snd)
	{
		return
			((fst instanceof Value) && (snd instanceof Value))
			? ((Value)fst).equal((Value)snd)
			: fst.equals(snd);
	}


	public static List toList(Object o)
	throws ListExpected
	{
		try { return ((Value)o).toList(); }
		catch (ClassCastException e) { throw new ListExpected(o); }
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

	public static Symbol toSymbol(Object o)
	throws SymbolExpected
	{
		try	{ return (Symbol)o; }
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

	public static ScmChar toScmChar(Object o)
	throws CharExpected
	{
		try	{ return (ScmChar)o; }
		catch (ClassCastException e) { throw new CharExpected(o); }
	}

	public static ScmString toScmString(Object o)
	throws StringExpected
	{
		try	{ return (ScmString)o; }
		catch (ClassCastException e) { throw new StringExpected(o); }
	}

	public static ScmVector toScmVector(Object o)
	throws VectorExpected
	{
		try	{ return (ScmVector)o; }
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

	public static boolean isList(Object o)
	{
		return (o instanceof Value) && ((Value)o).isList();
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
		return o instanceof Symbol;
	}

	public static boolean isScmNumber(Object o)
	{
		return o instanceof ScmNumber;
	}

	public static boolean isScmChar(Object o)
	{
		return o instanceof ScmChar;
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

	public static void display(Writer destination, Object o) throws IOException
	{
		if (o instanceof Value)
		{
			((Value)o).displayOn(destination);
		}
		else if (isScmBoolean(o))
		{
			destination.write(
				isTrue(o)
				? "#t"
				: "#f");
		}
		else
		{
			destination.write("#[" + o.toString() + "]");
		}
	}

	public static void write(Writer destination, Object o) throws IOException
	{
		if (o instanceof Value)
		{
			((Value)o).writeOn(destination);
		}
		else
		{
			display(destination, o);
		}
	}

	public static Object getConst(Object o)
	{
		if (o instanceof Value)
		{
			return ((Value)o).getConst();
		}
		else
		{
			return o;
		}
	}

	public static Object getCompiled(
		StaticEnvironment compilationEnv,
		Object            object)
	throws SchemeException
	{
		if (object instanceof Value)
		{
			return ((Value)object).getCompiled(compilationEnv);
		}
		else
		{
			return object; 
		}
	}

	public static Syntax getSyntax(
		StaticEnvironment compilationEnv,
		Object            object)
	throws SchemeException
	{
		if (object instanceof Value)
		{
			return ((Value)object).getSyntax(compilationEnv);
		}
		else
		{
			return ProcedureCall.create(object);
		}
	}
}
