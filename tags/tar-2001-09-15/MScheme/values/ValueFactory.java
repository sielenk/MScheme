package MScheme.values;

import java.lang.reflect.Field;
import java.io.Reader;
import java.io.Writer;

import MScheme.Value;

import MScheme.exceptions.OpenException;
import MScheme.exceptions.FunctionNotFoundException;


public abstract class ValueFactory
{
    public final static String id
        = "$Id$";


    public static Empty createList()
    { return Empty.create(); }

    public static Pair createList(Value first)
    { return List.prepend(first, createList()); }

    public static Pair createList(Value first, Value second)
    { return List.prepend(first, createList(second)); }

    public static Pair createList(Value first, Value second, Value third)
    { return List.prepend(first, createList(second, third)); }

    
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