package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.exceptions.InputPortExpected;
import MScheme.exceptions.OutputPortExpected;
import MScheme.exceptions.CloseException;


public abstract class Port
    extends Value
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


    public InputPort toInput() throws InputPortExpected
    { throw new InputPortExpected(this); }

    public OutputPort toOutput() throws OutputPortExpected
    { throw new OutputPortExpected(this); }
    
    
    public abstract void close()
        throws CloseException;
}

