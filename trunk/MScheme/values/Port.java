package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.exceptions.InputPortExpectedException;
import MScheme.exceptions.OutputPortExpectedException;
import MScheme.exceptions.CloseException;


public abstract class Port
    extends SelfEvaluatingValue
{
    // specialisation of Value
    
    public boolean isPort()
    { return true; }
    
    public Port toPort()
    { return this; }
    
    public void write(Writer destination)
        throws IOException
    { destination.write("[port]"); }
        
    // port
    
    public boolean isInput()
    { return false; }
    
    public boolean isOutput()
    { return false; }


    public InputPort toInput() throws InputPortExpectedException
    { throw new InputPortExpectedException(this); }

    public OutputPort toOutput() throws OutputPortExpectedException
    { throw new OutputPortExpectedException(this); }
    
    
    public abstract void close()
        throws CloseException;
}

