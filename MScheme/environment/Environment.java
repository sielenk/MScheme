package MScheme.environment;

import java.io.Writer;
import java.io.IOException;
import java.util.Vector;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.machine.Registers;
import MScheme.values.*;
import MScheme.code.Code;
import MScheme.syntax.Syntax;
import MScheme.syntax.SyntaxFactory;
import MScheme.functions.BuiltinTable;
import MScheme.functions.Thunk;
import MScheme.functions.ValueThunk;
import MScheme.exceptions.*;


public class Environment
    extends Value
{
    // *******************************************************************

    public void write(Writer destination)
        throws IOException
    { destination.write("[environment]"); }

    public Environment toEnvironment()
    { return this; }

    // *******************************************************************

    private StaticEnvironment _bindings;
    private Vector[]          _data;

    // *******************************************************************

    private Environment(
        StaticEnvironment bindings,
        Vector[]          data
    )
    { _bindings = bindings; _data = data; }

    Environment()
    { this(new StaticEnvironment(), (Environment)null); }

    Environment(
        StaticEnvironment bindings,
        Environment       parent
    )
    {
        int level = bindings.getLevel();
    
        _bindings = bindings;
        _data     = new Vector[level + 1];

        if (level > 0) {
            if (parent._bindings != bindings.getParent()) {
                throw new RuntimeException(
                    "consistency failure: StaticEnvironment parent"
                );
            }
            
            System.arraycopy(
                parent._data, 0,
                _data, 0,
                level
            );
        }

        Vector newData = new Vector();
        newData.setSize(bindings.getSize());
        _data[level] = newData;
    }

    Environment(
        StaticEnvironment  bindings,
        Environment        parent,
        Arity              arity,
        List               values
    ) throws ListExpected
    {
        this(bindings, parent);

        Vector data = _data[_bindings.getLevel()];
        List   tail = values;

        for (int i = 0; i < arity.getMin(); i++)
        {
            data.setElementAt(
                tail.getHead(),
                i
            );
            
            tail = tail.getTail();
        }

        if (arity.allowMore()) {
            data.setElementAt(
                tail,
                arity.getMin()
            );
        }
    }


    private static Environment
        _implementationEnvironment = getSchemeReportEnvironment();

    public static Environment getImplementationEnvironment()
    { return _implementationEnvironment; }


    private static Value   _nullEnvironmentHook         = null;
    private static Value   _schemeReportEnvironmentHook = null;

    private static void initHooks()
    {
        if (getImplementationEnvironment() == null) {
	        return;
		}
		if (_nullEnvironmentHook != null) {
		    return;
		}

        try {
            getImplementationEnvironment().define(
    	        Symbol.create("unique-id"),
    		    new ValueThunk() {
                    protected Value checkedCall()
                    { return Symbol.createUnique(); }
			    }
        	);

            getImplementationEnvironment().define(
        	    Symbol.create("current-environment"),
        	    new Thunk() {
			        protected Code checkedCall(Registers registers)
                    { return registers.getEnvironment().getLiteral(); }
			    }
        	);

    		_nullEnvironmentHook =
		        new Machine(getImplementationEnvironment()).evaluate(
			        InputPort.create("bootstrap_null.scm").read()
			    );

			_schemeReportEnvironmentHook =
		        new Machine(getImplementationEnvironment()).evaluate(
			        InputPort.create("bootstrap_sre.scm").read()
			    );
    	}
    	catch (SchemeException e) {
    	    throw new RuntimeException(
    	        "unexpected SchemeError:\n"
    		    + e.toString()
    		);
    	}
	}

    private static void callHook(Environment env, Value hook)
    {
        if (hook != null) {
            Value null_buffer = _nullEnvironmentHook;
            Value  sre_buffer = _schemeReportEnvironmentHook;
			
	        try {
		        synchronized (Environment.class) {
                    _nullEnvironmentHook         = null;
                    _schemeReportEnvironmentHook = null;

        	        new Machine(env).evaluate(hook);
			    }
		    }
		    catch (SchemeException e) {
		        throw new RuntimeException(
			        "nullEnvironmentHook caused an exception:\n"
				    + e.toString()
			    );
		    }
		    finally {
                _nullEnvironmentHook         = null_buffer;
                _schemeReportEnvironmentHook =  sre_buffer;
		    }
    	}
    }
    
    public static Environment getEmpty()
    {
        initHooks();
        return new Environment();
	}

    public static Environment getNullEnvironment()
    {
        Environment result = getEmpty();

        try {
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
        catch (AlreadyBound e) {
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

        try {
            for (int i = 0; i < BuiltinTable.builtins.length; i++) {
                result.define(
                    Symbol.create(BuiltinTable.builtins[i].getName()),
                    BuiltinTable.builtins[i].getFunc()
                );
            }
        }
        catch (CompileError e) {
            throw new RuntimeException(
                "unexpected CompileError"
            );
        }

        callHook(result, _schemeReportEnvironmentHook);

        return result;
    }


    public StaticEnvironment getStatic()
    { return _bindings; }

    public Environment getParent()
    { return new Environment(_bindings.getParent(), _data); }

    public Environment newChild()
    { return newChild(_bindings.newChild()); }

    public Environment newChild(
        StaticEnvironment newFrame
    )
    { return new Environment(newFrame, this); }
    
    public Environment newChild(
        StaticEnvironment newFrame,
        Arity             arity,
        List              values
    ) throws ListExpected
    { return new Environment(newFrame, this, arity, values); }

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
        Vector data  = _data[key.getLevel()];
        int    index = key.getIndex();
    
        try {
            data.setElementAt(value, index);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            data.setSize(index + 1);
            data.setElementAt(value, index);
        }
    }

    public void assign(Symbol key, Value value)
        throws SymbolNotFoundException, UnexpectedSyntax
    { assign(_bindings.getCodeFor(key), value); }


    public Value lookup(Reference ref)
        throws UninitializedSymbolException
    {
        Value result;
        
        try {
            result = (Value)_data[
                ref.getLevel()
            ].get(
                ref.getIndex()
            );
        }
        catch (ArrayIndexOutOfBoundsException e) {
            result = null;
        }
    
        if (result == null) {
            throw new UninitializedSymbolException(ref.getSymbol());
        }
        
        return result;
    }

    public Value lookup(Symbol key)
        throws SymbolNotFoundException, 
               UnexpectedSyntax,
               UninitializedSymbolException
    { return lookup(_bindings.getCodeFor(key)); }

    // ***********************************************************************
}
