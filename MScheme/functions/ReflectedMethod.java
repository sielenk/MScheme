package MScheme.functions;

import java.lang.reflect.*;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.SchemeException;
import MScheme.exceptions.ArityException;


public class ReflectedMethod
    extends Function
{
    private final Arity  _arity;
    private final Method _javaMethod;
    

    public ReflectedMethod(Method javaMethod)
    {
        Class[] paramTypes = javaMethod.getParameterTypes();

        if (paramTypes.length == 1) {
            if (paramTypes[0] == Value.class) {
                _arity = Arity.exactly(1);
            } else {
                _arity = null;
            }
        } else {
            _arity = Arity.exactly(paramTypes.length);
        }

        _javaMethod = javaMethod;
    }

    final public Code call(Machine machine, List arguments)
        throws SchemeException
    {
        Value[] argumentArray;

        if (_arity == null) {
            argumentArray = new Value[1];
            argumentArray[0] = arguments;
        } else {
            int len = arguments.getLength();
        
            if (!_arity.isValid(len)) {
                throw new ArityException(arguments, _arity);
            }
        
            argumentArray = new Value[len];
            List current  = arguments;
            int  index    = 0;

            while (!current.isEmpty()) {
                argumentArray[index++] = current.getHead();
                current                = current.getTail();
            }
        }

        try {
            return machine.handleResult(
                (Value)_javaMethod.invoke(null, argumentArray)
            );
        }
        catch (IllegalArgumentException e) {
            System.err.println("\n*** " + arguments + " ***");
            System.err.println("\n*** " + argumentArray.length + " ***");
            throw e;
        }
        catch (IllegalAccessException e) {
            throw new RuntimeException(
                "unexpected IllegalAccessException in ReflectedMethod.call"
            );
        }
        catch (InvocationTargetException e) {
            Throwable t = e.getTargetException();

            if (!(t instanceof SchemeException)) {
                throw new RuntimeException(
                    "not a SchemeException thrown in ReflectedMethod.call"
                );
            }

            throw (SchemeException)e.getTargetException();
        }
    }
}
