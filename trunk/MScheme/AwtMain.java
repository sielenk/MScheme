package MScheme;

import java.io.Reader;
import java.io.Writer;

import java.awt.event.WindowListener;
import java.awt.event.WindowEvent;

import MScheme.values.InputPort;
import MScheme.machine.Machine;


public final class AwtMain
	implements WindowListener
{
    public final static String id
        = "$Id$";

	public void windowDeiconified(WindowEvent e) { }
	public void windowIconified  (WindowEvent e) { }

	public void windowActivated  (WindowEvent e) { }
	public void windowDeactivated(WindowEvent e) { }

	public void windowOpened(WindowEvent e) { }
	public void windowClosed(WindowEvent e) 
	{
		System.exit(0);
	}

	public void windowClosing(WindowEvent e) 
	{
		e.getWindow().dispose();
	}


	public static void main(String argv[])
		throws Exception
	{
        final StdioFrame frame = new StdioFrame();

        frame.addWindowListener(new AwtMain());
        frame.setSize(600, 400);
        frame.show();

        Machine.stdin  = frame.stdin ();
        Machine.stdout = frame.stdout();

        Machine machine = new Machine();

        for (int i = 0; i < argv.length; i++)
        {
            InputPort in = InputPort.create(argv[i]);

            System.out.println("parsing " + argv[i]);

            Value v;
            while ((v = in.read()) != in.EOF_VALUE)
            {
                machine.evaluate(v);
            }
        }
        
        frame.dispose();
	}
}
