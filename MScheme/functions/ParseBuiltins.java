package MScheme.functions;

import java.lang.reflect.*;
import java.util.Vector;

import java.io.*;

import MScheme.values.Value;
import MScheme.values.List;
import MScheme.values.Function;


public class ParseBuiltins
{
    private static boolean paramsValid(Class[] params)
    {
        if (params.length == 1) {
            return (params[0] == Value.class) || (params[0] == List.class);
        } else {
            for (int i = 0; i < params.length; i++) {
                if (params[i] != Value.class) {
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
            .append("    protected Value checkedCall(")
                .append(arguments).append(")\n")
            .append("        throws RuntimeError, TypeError\n")
            .append("    {\n")
            .append("        switch(_id) {\n");
    }

    private static String parseName(String name)
    {
        StringBuffer buf = new StringBuffer();

        for (int index = 0; index < name.length(); index++) {
            char c = name.charAt(index);

            if (c == '_') {
                buf.append(
                    (char)Integer.parseInt(
                        name.substring(index + 1, index + 3),
                        16
                    )
                );
                index += 2;
            } else {
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
            "Value fst",
            "Value fst, Value snd",
            "Value fst, Value snd, Value trd",
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
        for (int i = 0; i <= MAXARGS; i++) {
            arityCases[i] = new StringBuffer();
            initClass(
                arityCases[i],
                arityClass[i],
                arityClassBase[i],
                arityTypedArgs[i]
            );
        }

        table.append("    BuiltinTable[] builtins = {\n");

        for (int i = 0; i < fields.length; i++) {
            Field field = fields[i];
            int   mo    = field.getModifiers();

            if (!Modifier.isStatic(mo) || !Modifier.isPublic(mo)) {
                continue;
            }

            if (!Function.class.isAssignableFrom(field.getType())) {
                continue;
            }

            final String       schemeName = parseName(field.getName());

            table
                .append("        new BuiltinTableEntry(Builtins.")
                .append(field.getName())
                .append(", \"")
                .append(schemeName)
                .append("\"),\n");
        }

        for (int i = 0; i < methods.length; i++) {
            Method me = methods[i];
            int    mo = me.getModifiers();

            if (!Modifier.isStatic(mo) || !Modifier.isPublic(mo)) {
                continue;
            }

            if (!paramsValid(me.getParameterTypes())) {
                continue;
            }

            if (!Value.class.isAssignableFrom(me.getReturnType())) {
                continue;
            }

            final String       schemeName = parseName(me.getName());
            final String       hostClass;
            final String       arguments;
            final StringBuffer cases;

            if ((me.getParameterTypes().length == 1) &&
                (me.getParameterTypes()[0] == List.class)
            ) {
                cases     = baseCases;
                arguments = baseArgs;
                hostClass = baseClass;
            } else {
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
            .append("    public Code call(Machine machine, List args)\n")
            .append("        throws RuntimeError, TypeError\n")
            .append("    { return checkedCall(args).getLiteral(); }\n")
            .append("}\n\n");

        table.append("    };\n");

        out.write(
            "package MScheme.functions;\n" +
            "\n" +
            "/* --- DON'T EDIT --- */\n" +
            "/* this file is generated by MScheme.functions.ParseBuiltins */\n" +
            "\n" +
            "import MScheme.machine.Machine;\n" +
            "import MScheme.code.Code;\n" +
            "import MScheme.values.*;\n\n" +
            "import MScheme.exceptions.*;\n" +
            "\n\n"
        );

        out.write(baseCases.toString());
        for (int i = 0; i <= MAXARGS; i++) {
            arityCases[i]
                .append(defaultCase)
                .append("        }\n    }\n}\n\n");
            out.write(arityCases[i].toString());
        }

        out.write(
            "class BuiltinTableEntry\n" +
            "    implements BuiltinTable\n" +
            "{\n" +
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
