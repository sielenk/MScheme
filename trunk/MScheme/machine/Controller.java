package MScheme.machine;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.Code;

import MScheme.code.CodeList;
import MScheme.code.Application;
import MScheme.functions.UnaryFunction;

import MScheme.exceptions.SchemeException;
import MScheme.exceptions.RuntimeError;


final class Subcontinuation
    extends UnaryFunction
{
    /** The CVS id of the file containing this class. */
    public final static String id
        = "$Id$";


    private final Continuation _leaf;

    Subcontinuation(
        Continuation rootsParent,
        Continuation leaf
    )
    {
        _leaf = Continuation.copyAndPrependSubcontinuation(
            rootsParent,
            leaf,
            null
        );
    }


    // implementation of Value

    public void write(Writer destination)
        throws IOException
    {
        destination.write(
            "#[subcontinuation]"
        );
    }


    // implementation of UnaryFunction

    protected Code checkedCall(Registers state, Value argument)
    {
        Continuation target = Continuation.copyAndPrependSubcontinuation(
            null,
            _leaf,
            state.getContinuation()
        );

        return new ContinuationFunction(
            target
        ).checkedCall(
            state,
            argument
        );
    }
}


final class RootContinuation
    extends Continuation
{
    private Object _equalityTag;


    Continuation cloneContinuation()
    {
        RootContinuation clonedRoot
            = (RootContinuation)super.cloneContinuation();

        clonedRoot._equalityTag = _equalityTag;
        return clonedRoot;
    }

    RootContinuation(Registers state)
    {
        super(state);
        _equalityTag = new Object();
    }

    protected Code execute(Registers state, Value result)
        throws SchemeException
    {
        return result.getLiteral();
    }

    protected String debugString()
    {
        return "spawn root";
    }


    public boolean equals(Object other)
    {
        try {
            return _equalityTag == ((RootContinuation)other)._equalityTag;
        }
        catch (ClassCastException e)
        { }
        
        return false;
    }
}


public final class Controller
    extends UnaryFunction
{
    /** The CVS id of the file containing this class. */
    public final static String id
        = "$Id$";


    private final RootContinuation _root;

    private Controller(RootContinuation root)
    {
        _root = root;
    }

    static Controller create(Registers state)
    {
        return new Controller(
            new RootContinuation(state)
        );
    }

    // implementation of Value

    public void write(Writer destination)
        throws IOException
    {
        destination.write(
            "#[subcomputation controller]"
        );
    }


    // implementation of UnaryFunction

    protected Code checkedCall(Registers state, Value argument)
        throws RuntimeError
    {
        Continuation current = state.getContinuation();
        while (!current.equals(_root))
        {
            current = current.getParent();

            if (current == null)
            {
                throw new RuntimeError(
                    this,
                    "invalid subcomputation controller use"
                );
            }
        }

        return Application.create(
            CodeList.create(
                argument.getLiteral(),
                new ContinuationFunction(
                    _root.getParent()
                ).checkedCall(
                    state,
                    new Subcontinuation(
                        _root.getParent(),
                        state.getContinuation()
                    )
                )
            )
        );
    }
}
