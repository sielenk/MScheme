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
    { return SchemeBoolean.createTrue(); }

    public static SchemeBoolean createFalse()
    { return SchemeBoolean.createFalse(); }
    
    public static SchemeBoolean createBool(boolean flag)
    { return SchemeBoolean.create(flag); }


    public static List prepend(Value head, List tail)
    { return List.prepend(head, tail); }
    
    public static List createList()
    { return List.with(); }

    public static List createList(Value first)
    { return List.with(first); }

    public static List createList(Value first, Value second)
    { return List.with(first, second); }

    public static List createList(Value first, Value second, Value third)
    { return List.with(first, second, third); }

    
    public static Symbol createSymbol(String javaString)
    { return Symbol.create(javaString); }

    
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
    { return SchemeNumber.create(value); }
    
    public static SchemeChar createChar(char c)
    { return SchemeChar.create(c); }
    
    public static SchemeString createString(String javaString)
    { return SchemeString.create(javaString); }

    public static SchemeVector createVector()
    { return SchemeVector.create(0); }

    public static SchemeVector createVector(int size)
    { return SchemeVector.create(size); }

    public static SchemeVector createVector(int size, Value fill)
    { return SchemeVector.create(size, fill); }

    
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

