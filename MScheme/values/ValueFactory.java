package MScheme.values;

import java.lang.reflect.Field;
import java.io.Reader;
import java.io.Writer;

import MScheme.values.Value;
import MScheme.exceptions.OpenException;
import MScheme.exceptions.FunctionNotFoundException;


public abstract class ValueFactory
{
    public static Pair prepend(Value head, List tail)
    { return Pair.create(head, tail.toValue()); }

    public static Empty createList()
    { return Empty.create(); }

    public static Pair createList(Value first)
    { return prepend(first, createList()); }

    public static Pair createList(Value first, Value second)
    { return prepend(first, createList(second)); }

    public static Pair createList(Value first, Value second, Value third)
    { return prepend(first, createList(second, third)); }

    
    public static Function createFunction(String name)
        throws FunctionNotFoundException
    {
        try {
            String className     = "MScheme.functions." + name + "Function";
            Class  classObject   = Class.forName(className);
            Field  instanceField = classObject.getField("INSTANCE");
            Object instance      = instanceField.get(null);
        
            return (Function)instance;
        }
        catch (Exception e) {
            throw new FunctionNotFoundException(name);
        }
    }
}

