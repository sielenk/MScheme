package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.environment.StaticEnvironment;
import MScheme.values.Value;
import MScheme.code.*;
import MScheme.exceptions.*;


public final class SchemeVector
    extends Value
{
    private final static SchemeVector _empty = new SchemeVector(0, null);
    
    public static SchemeVector getInstance(int size, Value fill)
    {
        return (size == 0) ? _empty : new SchemeVector(size, fill);
    }
    
    private final Value[] _data;
    
    private SchemeVector(int size, Value fill)
    {
        _data = new Value[size];
        
        for (int i = 0; i < _data.length; i++) {
            _data[i] = fill;
        }
    }
    

    public int getLength()
    { return _data.length; }


    public Value get(int index)
        throws VectorException
    {
        try {
            Value result = _data[index];
        
            if (result == null) {
                throw new UninitializedVectorException(this, index);
            }
            
            return result;
        }
        catch (ArrayIndexOutOfBoundsException e) {
            throw new InvalidVectorIndexException(this, index);
        }
    }

    public void set(int index, Value value)
        throws InvalidVectorIndexException
    {
        try {
            _data[index] = value;
        }
        catch (ArrayIndexOutOfBoundsException e) {
            throw new InvalidVectorIndexException(this, index);
        }
    }

    
    // specialisation of Value
    
    public boolean isVector()
    { return true; }
    
    public boolean equal(Value other)
    {
        try {
            SchemeVector otherVector = (SchemeVector)other;
        
            if (_data.length == otherVector._data.length) {
                for (int i = 0; i < _data.length; i++) {
                    if (!_data[i].equal(otherVector._data[i])) {
                        return false;
                    }
                }
                
                return true;
            }
        }
        catch (ClassCastException e) { }
        
        return false;
    }

    private void put(Writer destination, boolean doDisplay)
        throws IOException
    {
        destination.write("#(");
        for (int i = 0; i < getLength(); i++) {
            if (i > 0) {
                destination.write(' ');
            }
            if (doDisplay) {
                _data[i].display(destination);
            } else {
                _data[i].write(destination);
            }
        }
        destination.write(')');
    }

    public void write(Writer destination)
        throws IOException
    { put(destination, false); }

    public void display(Writer destination)
        throws IOException
    { put(destination, true); }


    public Code getCode(StaticEnvironment e)
        throws CantCompileException
    { throw new CantCompileException(this); }
}

