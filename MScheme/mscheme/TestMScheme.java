/* Toplevel test class for the juint test framework.
   Copyright (C) 2001  Marvin H. Sielenkemper

This file is part of MScheme.

MScheme is free software; you can redistribute it and/or modify 
it under the terms of the GNU General Public License as published by 
the Free Software Foundation; either version 2 of the License, 
or (at your option) any later version. 

MScheme is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details. 

You should have received a copy of the GNU General Public License
along with MScheme; see the file COPYING. If not, write to 
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA  02111-1307, USA. */

package mscheme;

import junit.framework.Test;
import junit.framework.TestSuite;

public class TestMScheme
{
    public final static String id
       = "$Id$";


    public static Test suite()
    {
        TestSuite suite = new TestSuite("All MScheme Tests");

        suite.addTestSuite(mscheme.util.TestArity.class);

        suite.addTestSuite(mscheme.values.TestList.class);
        suite.addTestSuite(mscheme.values.TestInputPort.class);

        suite.addTestSuite(mscheme.environment.TestEnvironment.class);

        suite.addTestSuite(mscheme.tests.TestR5RS.class);
        suite.addTestSuite(mscheme.tests.TestMachine.class);

        suite.addTestSuite(mscheme.tests.TestValue.class);
        suite.addTestSuite(mscheme.tests.TestBugs.class);

        return suite;
    }

    public static void main (String[] args)
    {
        junit.textui.TestRunner.run(
            suite()
        );
    }
}
