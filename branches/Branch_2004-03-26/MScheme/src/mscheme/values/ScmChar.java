/*
 * Created on 15.02.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values;

import java.io.IOException;
import java.io.Writer;

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public final class ScmChar
{
    static void outputOn(
    	Character o,
    	Writer destination,
    	boolean doWrite)
    throws IOException
    {
    	char c = o.charValue();
    	
        if (doWrite)
        {
            destination.write("#\\");
            switch (c)
            {
            case ' ' :
                destination.write("space");
                break;
        
            case '\n' :
                destination.write("newline");
                break;
        
            default :
                destination.write(c);
                break;
            }
        }
        else
        {
            destination.write(c);
        }
    }
}
