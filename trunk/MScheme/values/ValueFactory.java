package MScheme.values;

import java.lang.reflect.Field;
import java.io.Reader;
import java.io.Writer;

import MScheme.values.Value;
import MScheme.exceptions.OpenException;
import MScheme.exceptions.FunctionNotFoundException;


public abstract class ValueFactory
{
    public static SchemeBoolean createTrue()
    { return SchemeBoolean.True; }

    public static SchemeBoolean createFalse()
    { return SchemeBoolean.False; }
    
    public static SchemeBoolean createBool(boolean flag)
    { return flag ? createTrue() : createFalse(); }


    public static Pair createPair(Value first, Value second)
    { return new Pair(first, second); }


    public static List prepend(Value head, List tail)
    { return new Pair(head, tail); }
    
    public static List createList()
    { return Empty.INSTANCE; }

    public static List createList(Value first)
    { return prepend(first, createList()); }

    public static List createList(Value first, Value second)
    { return prepend(first, createList(second)); }

    public static List createList(Value first, Value second, Value third)
    { return prepend(first, createList(second, third)); }

    
    public static Symbol createSymbol(String symbol)
    { return new Symbol(symbol); }

    
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
    
    
    public static SchemeNumber createNumber(int value)
    { return new SchemeNumber(value); }
    
    public static SchemeCharacter createCharacter(char c)
    { return new SchemeCharacter(c); }
    
    public static SchemeString createString(String s)
    { return new SchemeString(s); }
    
    public static SchemeVector createVector()
    { return SchemeVector.getInstance(0, null); }

    public static SchemeVector createVector(int size)
    { return SchemeVector.getInstance(size, null); }

    public static SchemeVector createVector(int size, Value fill)
    { return SchemeVector.getInstance(size, fill); }

    
    public static InputPort createInputPort()
    { return InputPort.create(); }
    
    public static InputPort createInputPort(String filename)
        throws OpenException
    { return InputPort.create(filename); }
    
    public static InputPort createInputPort(Reader source)
    { return InputPort.create(source); }
    
    public static OutputPort createOutputPort()
    { return OutputPort.create(); }
    
    public static OutputPort createOutputPort(String filename)
        throws OpenException
    { return OutputPort.create(filename); }
    
    public static OutputPort createOutputPort(Writer destination)
    { return OutputPort.create(destination); }
}

