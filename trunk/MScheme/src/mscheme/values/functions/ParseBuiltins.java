/* A helper class to create BuiltinTable.java from Builtins.class.
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

package mscheme.values.functions;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import mscheme.values.Function;
import mscheme.values.List;


public class ParseBuiltins
{
    public final static String CVS_ID
        = "$Id$";


    private static boolean paramsValid(Class[] params)
    {
        if (params.length == 1)
        {
            return (params[0] == Object.class) || (params[0] == List.class);
        }
        else
        {
            for (int i = 0; i < params.length; i++)
            {
                if (params[i] != Object.class)
                {
                    return false;
                }
            }
        }

        return true;
    }

    private static void initClass(
        StringBuffer cases,
        String       className,
        String       baseClassName,
        String       arguments
    )
    {
        cases
        .append("class ").append(className).append("\n")
        .append("    extends    ").append(baseClassName).append("\n")
        .append("    implements BuiltinTable\n")
        .append("{\n")
        .append("    public final static String CVS_ID\n")
        .append("        = \"$Id$\";\n")
        .append("\n")
        .append("\n")
        .append("    private final int    _id;\n")
        .append("    private final String _name;\n")
        .append("\n")
        .append("    ").append(className).append("(int id, String name)\n")
        .append("    { _id = id; _name = name; }\n")
        .append("\n")
        .append("    public String getName() { return _name; }\n")
        .append("\n")
        .append("    public Function getFunc() { return this; }\n")
        .append("\n")
        .append("    public void write(Writer destination)\n")
        .append("        throws IOException\n")
        .append("    {\n")
        .append("        destination.write(\n")
        .append("            \"#[primitive \" + _id + \": \" + _name + \"]\"\n")
        .append("        );\n")
        .append("    }\n")
        .append("\n")
        .append("    protected Object checkedCall(")
        .append(arguments).append(")\n")
        .append("        throws SchemeException\n")
        .append("    {\n")
        .append("        switch(_id) {\n");
    }

    private static String parseName(String name)
    {
        StringBuffer buf = new StringBuffer();

        for (
            int index = name.startsWith("__") ? 2 : 0;
            index < name.length();
            ++index
        )
        {
            char c = name.charAt(index);

            if (c == '_')
            {
                buf.append(
                    (char)Integer.parseInt(
                        name.substring(index + 1, index + 3),
                        16
                    )
                );
                index += 2;
            }
            else
            {
                buf.append(c);
            }
        }

        return buf.toString();
    }

    private static void parseClass(Class cls, Writer out)
    throws IOException
    {
        final String   baseClass  = "BuiltinRaw";
        final String[] arityClass = {
                                        "BuiltinThunks",
                                        "BuiltinUnary",
                                        "BuiltinBinary",
                                        "BuiltinTernary",
                                    };
        final String   baseClassBase  = "Function";
        final String[] arityClassBase = {
                                            "ValueThunk",
                                            "UnaryValueFunction",
                                            "BinaryValueFunction",
                                            "TernaryValueFunction",
                                        };

        final String   baseArgs  = "args";
        final String[] arityArgs = {
                                       "",
                                       "fst",
                                       "fst, snd",
                                       "fst, snd, trd",
                                   };
        final String   baseTypedArgs  = "List args";
        final String[] arityTypedArgs = {
                                            "",
                                            "Object fst",
                                            "Object fst, Object snd",
                                            "Object fst, Object snd, Object trd",
                                        };
        final int MAXARGS = arityArgs.length - 1;

        final String defaultCase =
            "        default:\n" +
            "            throw new RuntimeException(\"unexpected Id\");\n";

        StringBuffer   baseCases     = new StringBuffer();
        StringBuffer[] arityCases    = new StringBuffer[MAXARGS + 1];
        StringBuffer   table         = new StringBuffer();
        Method[]       methods       = cls.getMethods();
        Field[]        fields        = cls.getFields();
        int            functionIndex = 0;

        initClass(baseCases, baseClass, baseClassBase, baseTypedArgs);
        for (int i = 0; i <= MAXARGS; i++)
        {
            arityCases[i] = new StringBuffer();
            initClass(
                arityCases[i],
                arityClass[i],
                arityClassBase[i],
                arityTypedArgs[i]
            );
        }

        table.append("    BuiltinTable[] builtins = {\n");

        for (int i = 0; i < fields.length; i++)
        {
            Field field = fields[i];
            int   mo    = field.getModifiers();

            if (!Modifier.isStatic(mo) || !Modifier.isPublic(mo))
            {
                continue;
            }

            if (!Function.class.isAssignableFrom(field.getType()))
            {
                continue;
            }

            final String schemeName = parseName(field.getName());

            table
            .append("        new BuiltinTableEntry(Builtins.")
            .append(field.getName())
            .append(", \"")
            .append(schemeName)
            .append("\"),\n");
        }

        for (int i = 0; i < methods.length; i++)
        {
            Method me = methods[i];
            int    mo = me.getModifiers();

            if (!Modifier.isStatic(mo) || !Modifier.isPublic(mo))
            {
                continue;
            }

            if (!paramsValid(me.getParameterTypes()))
            {
                continue;
            }

            if (!Object.class.isAssignableFrom(me.getReturnType()))
            {
                continue;
            }

            final String       schemeName = parseName(me.getName());
            final String       hostClass;
            final String       arguments;
            final StringBuffer cases;

            if ((me.getParameterTypes().length == 1) &&
                    (me.getParameterTypes()[0] == List.class)
               )
            {
                cases     = baseCases;
                arguments = baseArgs;
                hostClass = baseClass;
            }
            else
            {
                int arity = me.getParameterTypes().length;

                cases     = arityCases[arity];
                arguments = arityArgs [arity];
                hostClass = arityClass[arity];
            }


            table
            .append("        new ")
            .append(hostClass)
            .append('(')
            .append(functionIndex)
            .append(", \"")
            .append(schemeName)
            .append("\"),\n");

            cases
            .append("        case ")
            .append(functionIndex)
            .append(": // ")
            .append(schemeName)
            .append("\n")
            .append("            return Builtins.")
            .append(me.getName())
            .append("(")
            .append(arguments)
            .append(");\n\n");

            functionIndex++;
        }

        baseCases
        .append(defaultCase)
        .append("        }\n")
        .append("    }\n")
        .append("\n")
        .append("    protected Arity getArity()\n")
        .append("    { return Arity.atLeast(0); }\n")
        .append("\n")
        .append("    public Object call(Registers state, List args)\n")
        .append("        throws SchemeException\n")
        .append("    { return checkedCall(args); }\n")
        .append("}\n\n");

        table.append("    };\n");

        out.write(
            "package mscheme.values.functions;\n" +
            "\n" +
            "/* --- DON'T EDIT --- */\n" +
            "/* this file is generated by mscheme.values.functions.ParseBuiltins */\n" +
            "\n" +
            "import java.io.Writer;\n" +
            "import java.io.IOException;\n" +
            "\n" +
            "import mscheme.util.Arity;\n" +
			"import mscheme.machine.Registers;\n" +
            "\n" +
            "import mscheme.values.*;\n\n" +
            "import mscheme.exceptions.*;\n" +
            "\n\n"
        );

        out.write(baseCases.toString());
        for (int i = 0; i <= MAXARGS; i++)
        {
            arityCases[i]
            .append(defaultCase)
            .append("        }\n    }\n}\n\n");
            out.write(arityCases[i].toString());
        }

        out.write(
            "class BuiltinTableEntry\n" +
            "    implements BuiltinTable\n" +
            "{\n" +
            "    public final static String id\n" +
            "        = \"$Id$\";\n" +
            "\n" +
            "    private final Function _func;\n" +
            "    private final String   _name;\n" +
            "\n" +
            "    BuiltinTableEntry(Function func, String name)\n" +
            "    { _func = func; _name = name; }\n" +
            "\n" +
            "    public String getName() { return _name; }\n" +
            "\n" +
            "    public Function getFunc() { return _func; }\n" +
            "}\n" +
            "\n\n" +
            "public interface BuiltinTable\n" +
            "{\n" +
            "    String id\n" +
            "        = \"$Id$\";\n" +
            "\n" +
            "\n" +
            "    String getName();\n" +
            "    Function getFunc();\n\n"
        );
        out.write(table.toString());
        out.write("}");
    }


    public static void main(String[] argv)
    throws IOException
    {
        Writer out = new OutputStreamWriter(System.out);

        parseClass(
            argv.length > 0
            ? Object.class
            : Builtins.class,
            out
        );

        out.flush();
    }
}
