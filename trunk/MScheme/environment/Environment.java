package MScheme.environment;

import java.io.Writer;
import java.io.IOException;
import java.util.Vector;

import MScheme.Value;
import MScheme.List;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.util.Arity;

import MScheme.machine.Machine;
import MScheme.machine.Registers;

import MScheme.values.*;

import MScheme.syntax.SyntaxFactory;

import MScheme.functions.BuiltinTable;
import MScheme.functions.Thunk;
import MScheme.functions.ValueThunk;
import MScheme.functions.TernaryValueFunction;
import MScheme.functions.YCombinator;

import MScheme.exceptions.*;


class UniqueId
    extends ValueThunk
{
    public final static String id
        = "$Id$";


    public final static UniqueId INSTANCE
        = new UniqueId();

    private UniqueId()
    { }

    protected Value checkedCall()
    {
        return Symbol.createUnique();
    }
}
 
class CurrentEnvironment
    extends Thunk
{
    public final static String id
        = "$Id$";


    public final static CurrentEnvironment INSTANCE
        = new CurrentEnvironment();

    private CurrentEnvironment()
    { }

    protected Code checkedCall(Registers state)
    {
        return state
            .getEnvironment()
            .getLiteral();
    }
}


public final class Environment
    extends ValueDefaultImplementations
{
    public final static String id
        = "$Id$";


    // *******************************************************************

    public void write(Writer destination)
        throws IOException
    {
        destination.write("[environment]");
    }

    public Environment toEnvironment()
    {
        return this;
    }

    // *******************************************************************

    private final StaticEnvironment _bindings;
    private final Vector[]          _frames;

    // *******************************************************************

    private Environment(
        StaticEnvironment bindings,
        Vector[]          frames
    )
    {
        _bindings = bindings;
        _frames   = frames;
    }
 
    private static Environment create()
    {
        return create(new StaticEnvironment(), null);
    }

    private static Environment create(
        StaticEnvironment bindings,
        Environment       parent
    )
    {
        int      level  = bindings.getLevel();
        Vector[] frames = new Vector[level + 1];

        if (level > 0)
        {
            if (parent._bindings != bindings.getParent())
            {
                throw new RuntimeException(
                   "consistency failure: StaticEnvironment parent"
                );
            }

            System.arraycopy(
                parent._frames, 0,
                frames, 0,
                level
            );
        }

        {
            Vector locations = new Vector();
            locations.setSize(bindings.getSize());
            frames[level] = locations;
        }

        return new Environment(bindings, frames);
    }

    private static Environment create(
        StaticEnvironment  bindings,
        Environment        parent,
        Arity              arity,
        List               values
    ) throws PairExpected, ListExpected
    {
        Environment result = create(bindings, parent);

        Vector locations = result._frames[result._bindings.getLevel()];
        List   rest      = values;

        for (int i = 0; i < arity.getMin(); i++)
        {
            locations.setElementAt(
                rest.getHead(),
                i
            );

            rest = rest.getTail();
        }

        if (arity.allowMore())
        {
            locations.setElementAt(
                rest,
                arity.getMin()
            );
        }

        return result;
    }


    private static Environment
        _implementationEnvironment = getSchemeReportEnvironment();

    public static Environment getImplementationEnvironment()
    {
        return _implementationEnvironment;
    }


    private static Value   _nullEnvironmentHook         = null;
    private static Value   _schemeReportEnvironmentHook = null;

    private static void initHooks()
    {
        if (_implementationEnvironment == null)
        {
            return;
        }
        if (_nullEnvironmentHook != null)
        {
            return;
        }

        try
        {
            _implementationEnvironment.define(
                Symbol.create("unique-id"),
                UniqueId.INSTANCE
            );

            _implementationEnvironment.define(
                Symbol.create("current-environment"),
                CurrentEnvironment.INSTANCE
            );

            _implementationEnvironment.define(
                Symbol.create("y"),
                YCombinator.INSTANCE
            );

            _nullEnvironmentHook =
                Machine.evaluate(
                    InputPort.create("bootstrap_null.scm").read(),
                    _implementationEnvironment
                );

            _schemeReportEnvironmentHook =
                Machine.evaluate(
                    InputPort.create("bootstrap_sre.scm").read(),
                    _implementationEnvironment
                );
        }
        catch (SchemeException e)
        {
            throw new RuntimeException(
                      "unexpected SchemeError:\n"
                      + e.toString()
                  );
        }
    }

    private static void callHook(Environment env, Value hook)
    {
        if (hook != null)
        {
            Value null_buffer = _nullEnvironmentHook;
            Value  sre_buffer = _schemeReportEnvironmentHook;

            try
            {
                synchronized (Environment.class)
                {
                    _nullEnvironmentHook         = null;
                    _schemeReportEnvironmentHook = null;

                    Machine.evaluate(hook, env);
                }
            }
            catch (SchemeException e)
            {
                throw new RuntimeException(
                          "nullEnvironmentHook caused an exception:\n"
                          + e.toString()
                      );
            }
            finally
            {
                _nullEnvironmentHook         = null_buffer;
                _schemeReportEnvironmentHook =  sre_buffer;
            }
        }
    }

    public static Environment getEmpty()
    {
        initHooks();
        return create();
    }

    public static Environment getNullEnvironment()
    {
        Environment result = getEmpty();

        try
        {
            StaticEnvironment staticBindings = result.getStatic();

            staticBindings.defineSyntax(
                Symbol.create("quote"),
                SyntaxFactory.getQuoteToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("if"),
                SyntaxFactory.getIfToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("begin"),
                SyntaxFactory.getBeginToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("lambda"),
                SyntaxFactory.getLambdaToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("let"),
                SyntaxFactory.getLetToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("let*"),
                SyntaxFactory.getLetStarToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("letrec"),
                SyntaxFactory.getLetrecToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("define"),
                SyntaxFactory.getDefineToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("set!"),
                SyntaxFactory.getSetToken()
            );
            staticBindings.defineSyntax(
                Symbol.create("define-syntax"),
                SyntaxFactory.getDefineSyntaxToken()
            );
        }
        catch (AlreadyBound e)
        {
            throw new RuntimeException(
                "unexpected AlreadyBound in getNullEnvironment()"
            );
        }

        callHook(result, _nullEnvironmentHook);

        return result;
    }

    public static Environment getSchemeReportEnvironment()
    {
        Environment result = getNullEnvironment();

        try
        {
            for (int i = 0; i < BuiltinTable.builtins.length; i++)
            {
                result.define(
                    Symbol.create(BuiltinTable.builtins[i].getName()),
                    BuiltinTable.builtins[i].getFunc()
                );
            }
        }
        catch (CompileError e)
        {
            throw new RuntimeException(
                      "unexpected CompileError"
                  );
        }

        callHook(result, _schemeReportEnvironmentHook);

        return result;
    }


    public StaticEnvironment getStatic()
    {
        return _bindings;
    }

    public Environment getParent()
    {
        return new Environment(_bindings.getParent(), _frames);
    }

    public Environment newChild()
    {
        return newChild(_bindings.newChild());
    }

    public Environment newChild(
        StaticEnvironment newFrame
    )
    {
        return create(newFrame, this);
    }

    public Environment newChild(
        StaticEnvironment newFrame,
        Arity             arity,
        List              values
    ) throws ListExpected, PairExpected
    {
        return create(newFrame, this, arity, values);
    }

    // *** Envrionment access ************************************************

    // *** code access (compiletime) ***

    public Reference define(Symbol key, Value value)
    throws CompileError
    {
        Reference newReference = _bindings.define(key);
        assign(newReference, value);
        return newReference;
    }

    // *** value access (runtime) ***

    public void assign(Reference key, Value value)
    {
        Vector locations = _frames[key.getLevel()];
        int    index     = key.getIndex();

        try
        {
            locations.setElementAt(value, index);
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            locations.setSize(index + 1);
            locations.setElementAt(value, index);
        }
    }

    public void assign(Symbol key, Value value)
        throws SymbolNotFoundException, UnexpectedSyntax
    {
        assign(_bindings.getReferenceFor(key), value);
    }


    public Value lookup(Reference ref)
    throws UninitializedSymbolException
    {
        Value result;

        try
        {
            result = (Value)_frames[
                         ref.getLevel()
                     ].get(
                         ref.getIndex()
                     );
        }
        catch (ArrayIndexOutOfBoundsException e)
        {
            result = null;
        }

        if (result == null)
        {
            throw new UninitializedSymbolException(ref.getSymbol());
        }

        return result;
    }

    public Value lookup(Symbol key)
    throws SymbolNotFoundException,
                UnexpectedSyntax,
                UninitializedSymbolException
    {
        return lookup(_bindings.getReferenceFor(key));
    }

    // ***********************************************************************
}
