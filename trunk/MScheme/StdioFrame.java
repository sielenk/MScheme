package MScheme;

import java.awt.Frame;
import java.awt.Button;
import java.awt.Panel;
import java.awt.TextField;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.io.Reader;
import java.io.Writer;
import java.io.IOException;


public class StdioFrame
    extends Frame
    implements ActionListener
{
    private StdioArea _stdio;
    private Button    _cutB, _copyB, _pasteB;

    /***** interface ActionListener begin *****/
    
    public void actionPerformed(ActionEvent e) 
    {
        String arg = e.getActionCommand();
        
        if (arg.equals("Clear"))
        {
            _stdio.clear();
        }
        else if (arg.equals(_copyB.getActionCommand()))
        {
            _stdio.copy();
        }
        else if (arg.equals(_cutB.getActionCommand())) 
        {
            _stdio.cut();
        }
        else if (arg.equals(_pasteB.getActionCommand()))
        {
            _stdio.paste();
        }
        else if (arg.equals(_stdio.getActionCommand()))
        {
            _pasteB.setEnabled(_stdio.canPaste());

            boolean readEnable = _stdio.canCopyCut();

            _copyB.setEnabled(readEnable);
             _cutB.setEnabled(readEnable);
        }
    }

    /***** interface ActionListener end *****/

    /***** applet initialisation function begin *****/
     
    public StdioFrame()
    {
        super("StdioFrame");

        {
            Panel p = new Panel();

            Button clearB = new Button("Clear");
            clearB.addActionListener(this); 
            p.add(clearB);

            _copyB = new Button("Copy");
            _copyB.addActionListener(this);
            p.add(_copyB);

            _cutB = new Button("Cut");
            _cutB.addActionListener(this);
            p.add(_cutB);

            _pasteB = new Button("Paste");
            _pasteB.addActionListener(this);
            p.add(_pasteB);

            add(p, "South");
        }

        {
            _stdio = new StdioArea();
            _stdio.addActionListener(this);
            add(_stdio, "North");
        }
    }

    /***** applet initialisation function end *****/

    public Reader stdin () { return _stdio.stdin (); }
    public Writer stdout() { return _stdio.stdout(); }
}
