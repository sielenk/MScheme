package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.environment.StaticEnvironment;
import MScheme.values.Value; 
import MScheme.code.CodeList;
import MScheme.exceptions.*;


public abstract class List
    extends Value
{
    // specialisation of Value
    
    public abstract boolean isList();
    
    public final List toList()
    { return this; }


    private void put(Writer destination, boolean doDisplay)
        throws IOException
    {
        boolean first   = true;
        Value   current = this;
        
        destination.write('(');
        
        try {
            while (current.isPair()) {
                Pair pair = current.toPair();
        
                if (first) {
                    first = false;
                } else {
                    destination.write(' ');
                }
            
                if (doDisplay) {
                    pair.getFirst().display(destination);
                } else {
                    pair.getFirst().write(destination);
                }
                current = pair.getSecond();
            }
        }
        catch (PairExpectedException e) { }
        
        if (!current.isList()) {
            destination.write(" . ");
            if (doDisplay) {
                current.display(destination);
            } else {
                current.write(destination);
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


    // abstract list interface
    
    abstract public boolean isEmpty();
    
    abstract public int getLength()
        throws ListExpectedException;
        
    abstract public Value getHead() throws PairExpectedException;
    abstract public List  getTail() throws ListExpectedException;
        
    abstract public List getReversed() throws ListExpectedException;


    public abstract CodeList getCodeList(StaticEnvironment e)
        throws SchemeException;
}

