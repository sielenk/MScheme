package MScheme.environment;

import java.io.Writer;
import java.io.IOException;
import java.util.Vector;

import MScheme.util.Arity;
import MScheme.values.*;
import MScheme.code.Code;
import MScheme.syntax.Syntax;
import MScheme.syntax.SyntaxFactory;
import MScheme.functions.BuiltinTable;
import MScheme.exceptions.*;


public class DynamicEnvironment
    extends SelfEvaluatingValue
{
    // *******************************************************************

    public void write(Writer destination)
        throws IOException
    { destination.write("[environment]"); }

    public DynamicEnvironment toEnvironment()
    { return this; }

    // *******************************************************************

    private StaticEnvironment _bindings;
    private Vector[]          _data;

    // *******************************************************************

    private DynamicEnvironment(
        StaticEnvironment bindings,
        Vector[]          data
    )
    { _bindings = bindings; _data = data; }

    DynamicEnvironment()
    { this(new StaticEnvironment(), (DynamicEnvironment)null); }

    DynamicEnvironment(
        StaticEnvironment bindings,
        DynamicEnvironment parent
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

    DynamicEnvironment(
        StaticEnvironment  bindings,
        DynamicEnvironment parent,
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


    public static DynamicEnvironment getEmpty()
    { return new DynamicEnvironment(); }

    public static DynamicEnvironment getNullEnvironment()
    {
        DynamicEnvironment result = getEmpty();

        try {
            StaticEnvironment staticBindings = result.getStatic();

            staticBindings.defineSyntax(
                ValueFactory.createSymbol("quote"),
                SyntaxFactory.getQuoteToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("quasiquote"),
                SyntaxFactory.getQuasiquoteToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("cond"),
                SyntaxFactory.getCondToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("if"),
                SyntaxFactory.getIfToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("begin"),
                SyntaxFactory.getBeginToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("lambda"),
                SyntaxFactory.getLambdaToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("let"),
                SyntaxFactory.getLetToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("define"),
                SyntaxFactory.getDefineToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("set!"),
                SyntaxFactory.getSetToken()
            );
        }
        catch (AlreadyBound e) {
            throw new RuntimeException(
                "unexpected AlreadyBound in getNullEnvironment()"
            );
        }

        return result;
    }

    public static DynamicEnvironment getSchemeReportEnvironment()
    {
        DynamicEnvironment result = getNullEnvironment();

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

        return result;
    }


    public StaticEnvironment getStatic()
    { return _bindings; }

    public DynamicEnvironment getParent()
    { return new DynamicEnvironment(_bindings.getParent(), _data); }

    public DynamicEnvironment newChild()
    { return newChild(_bindings.newChild()); }

    public DynamicEnvironment newChild(
        StaticEnvironment newFrame
    )
    { return new DynamicEnvironment(newFrame, this); }
    
    public DynamicEnvironment newChild(
        StaticEnvironment newFrame,
        Arity             arity,
        List              values
    ) throws ListExpected
    { return new DynamicEnvironment(newFrame, this, arity, values); }

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
