package MScheme.values;

import java.io.Writer;
import java.io.FileWriter;
import java.io.OutputStreamWriter;
import java.io.IOException;

import MScheme.exceptions.OpenException;
import MScheme.exceptions.WriteException;
import MScheme.exceptions.CloseException;


public class OutputPort
    extends Value
{
    private final Writer _writer;
    
    private OutputPort(Writer writer)
    { _writer = writer; }


    public static OutputPort create(Writer writer)
    { return new OutputPort(writer); }

    public static OutputPort create(ScmString filename)
        throws OpenException
    { return create(filename.getJavaString()); }

    public static OutputPort create(String filename)
        throws OpenException
    {
        try {
            return create(new FileWriter(filename));
        }
        catch (IOException e) {
            throw new OpenException(
                ValueFactory.createString(filename)
            );
        }
    }

    public static OutputPort create()
    { return create(new OutputStreamWriter(System.out)); }
    

    // specialisation of Port
    
    public void write(Writer destination)
        throws IOException
    { destination.write("[output port]"); }
        
    public boolean isPort()
    { return true; }
    
    public OutputPort toOutputPort()
    { return this; }
    
    
    public void close()
        throws CloseException
    {
        try {
            _writer.close();
        }
        catch (IOException e) {
            throw new CloseException(this);
        }
    }


    // output port
    
    public void writeChar(char c)
        throws WriteException
    {
        try {
            _writer.write(c);
            _writer.flush();
        }
        catch (IOException e) {
            throw new WriteException(this);
        }
    }
    
    public void writeScmChar(ScmChar c)
        throws WriteException
    { writeChar(c.getJavaChar()); }
    
    public void write(Value datum)
        throws WriteException
    {
        try {
            datum.write(_writer);
            _writer.flush();
        }
        catch (IOException e) {
            throw new WriteException(this);
        }
    }

    public void display(Value datum)
        throws WriteException
    {
        try {
            datum.display(_writer);
            _writer.flush();
        }
        catch (IOException e) {
            throw new WriteException(this);
        }
    }
}
