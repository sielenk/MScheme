package MScheme.values;

import java.io.Writer;
import java.io.StringWriter;
import java.io.IOException;

import MScheme.Value;
import MScheme.List;
import MScheme.Translator;
import MScheme.Code;

import MScheme.code.Literal;

import MScheme.environment.Environment;
import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.*;


/**
 * This class is the base class for all scheme values.
 * Most of its methods provide a default implementation.
 */
public abstract class ValueImplementation
    implements Value
{
    /** The CVS id of the file containing this class. */
    public final static String id
        = "$Id$";

    /** The default constructor. */
    protected ValueImplementation()
    { }

    /**
     * Returns an immutable version of <code>this</code>.
     * <p>
     * @return always <code>this</code>.
     */
    public Value getConst() { return this; }

    /**
     * Returns Scheme's boolean interpretation of a value.
     * All values except <code>#f</code> are regarded as
     * true in Scheme.
     * <p>
     * @return always <code>true</code>
     */
    public boolean isTrue() { return true; }


    /**
     * Returns <code>true</code> for proper lists.
     * <p>
     * This function is opverridden in the classes
     * {@link Empty} and {@link Pair}.
     * <p>
     * @return always <code>false</code>
     */
    public boolean isList() { return false; }

    
    // disjoint types (as in R5RS, 3.2)

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmBoolean}.
     * <p>
     * But the default implementation always
     * @return <code>false</code>
     */
    public boolean isScmBoolean() { return false; }

    /**
     * Returns <code>true</code> for instances of 
     * {@link Pair}.
     * <p>
     * But the default implementation always
     * @return <code>false</code>
     */
    public boolean isPair() { return false; }

    /**
     * Returns <code>true</code> for instances of 
     * {@link Symbol}.
     * <p>
     * But the default implementation always
     * @return <code>false</code>
     */
    public boolean isSymbol() { return false; }

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmNumber}.
     * <p>
     * But the default implementation always
     * @return <code>false</code>
     */
    public boolean isScmNumber() { return false; }

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmChar}.
     * <p>
     * But the default implementation always
     * @return <code>false</code>
     */
    public boolean isScmChar()    { return false; }

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmString}.
     * <p>
     * But the default implementation always
     * @return <code>false</code>
     */
    public boolean isScmString()  { return false; }

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmVector}.
     * <p>
     * But the default implementation always
     * @return <code>false</code>
     */
    public boolean isScmVector()  { return false; }

    /**
     * Returns <code>true</code> for instances of 
     * {@link OutputPort} and {@link InputPort}.
     * <p>
     * But the default implementation always
     * @return <code>false</code>
     */
    public boolean isPort()       { return false; }

    /**
     * Returns <code>true</code> for instances of 
     * {@link Function}.
     * <p>
     * But the default implementation always
     * @return <code>false</code>
     */
    public boolean isFunction()   { return false; }


    // type casts (to get SchemeExceptions instead of ClassCastExceptions)
    
    /**
     * Casts a reference to {@link Value} to a reference to {@link List}.
     *
     * <p>These type cast functions are provided here,
     * because simple casting and catching a possible
     * {@link java.lang.ClassCastException} always unwinds the stack.
     * Due to the dynamic nature of Scheme
     * it is desireable to be able to start an error
     * session and query the user for a value of
     * appropriate type and return it where the
     * error occured.</p>
     *
     * <p>But for now these functions just throw an exception.</p>
     *
     * @return never
     * @throws ListExpected always
     */
    public List toList()
        throws ListExpected
    { throw new ListExpected(this); }

    /**
     * Casts a reference to {@link Value} to a reference to {@link Pair}.
     * <p>
     * @see  #toList()
     * @return never
     * @throws PairExpected always
     */
    public Pair toPair()
        throws PairExpected
    { throw new PairExpected(this); }

    /**
     * Casts a reference to {@link Value} to a reference to {@link Symbol}.
     * <p>
     * @see  #toList()
     * @return never
     * @throws SymbolExpected always
     */
    public Symbol toSymbol()
        throws SymbolExpected
    { throw new SymbolExpected(this); }

    /**
     * Casts a reference to {@link Value} to a reference to {@link ScmNumber}.
     * <p>
     * @see  #toList()
     * @return never
     * @throws NumberExpected always
     */
    public ScmNumber toScmNumber()
        throws NumberExpected
    { throw new NumberExpected(this); }

    /**
     * Casts a reference to {@link Value} to a reference to {@link ScmChar}.
     * <p>
     * @see  #toList()
     * @return never
     * @throws CharExpected always
     */
    public ScmChar toScmChar()
        throws CharExpected
    { throw new CharExpected(this); }

    /**
     * Casts a reference to {@link Value} to a reference to {@link ScmString}.
     * <p>
     * @see  #toList()
     * @return never
     * @throws StringExpected always
     */
    public ScmString toScmString()
        throws StringExpected
    { throw new StringExpected(this); }

    /**
     * Casts a reference to {@link Value} to a reference to {@link ScmVector}.
     * <p>
     * @see  #toList()
     * @return never
     * @throws VectorExpected always
     */
    public ScmVector toScmVector()
        throws VectorExpected
    { throw new VectorExpected(this); }

    /**
     * Casts a reference to {@link Value} to a reference to {@link InputPort}.
     * <p>
     * @see  #toList()
     * @return never
     * @throws InputPortExpected always
     */
    public InputPort toInputPort()
        throws InputPortExpected
    { throw new InputPortExpected(this); }

    /**
     * Casts a reference to {@link Value} to a reference to {@link OutputPort}.
     * <p>
     * @see  #toList()
     * @return never
     * @throws OutputPortExpected always
     */
    public OutputPort toOutputPort()
        throws OutputPortExpected
    { throw new OutputPortExpected(this); }

    /**
     * Casts a reference to {@link Value} to a reference to {@link Function}.
     * <p>
     * @see  #toList()
     * @return never
     * @throws FunctionExpected always
     */
    public Function toFunction()
        throws FunctionExpected
    { throw new FunctionExpected(this); }

    /**
     * Casts a reference to {@link Value} to a reference to {@link Environment}.
     * <p>
     * @see  #toList()
     * @return never
     * @throws EnvironmentExpected always
     */
    public Environment toEnvironment()
        throws EnvironmentExpected
    { throw new EnvironmentExpected(this); }

    /**
     * Casts a reference to {@link Value} to a reference to {@link StaticEnvironment}.
     * <p>
     * @see  #toList()
     * @return never
     * @throws EnvironmentExpected always
     */
    public StaticEnvironment toStaticEnvironment()
        throws EnvironmentExpected
    { throw new EnvironmentExpected(this); }


    // scheme equivalence predicates (as in R5RS, 6.1)

    /**
     * Compares two values for Scheme's <code>eq</code>-equality.
     * This is the most discriminating equality in Scheme and is
     * supposed to be easiest to check.
     * <p>
     * @param other the value with which to compare.
     * @return <code>this == other</code>
     */
    public boolean eq(Value other)
    { return this == other; }
    
    /**
     * Compares two values for Scheme's <code>eqv</code>-equality.
     * This equality is supposed to compare characters and numbers
     * of the same value as equal, even if they are not the same
     * instance.
     * <p>
     * @param other the value with which to compare.
     * @return <code>eq(other)</code>
     */
    public boolean eqv(Value other)
    { return eq(other); }
    
    /**
     * Compares two values for Scheme's <code>equal</code>-equality.
     * This equality compares compound values recursively, primitive
     * values like <code>eqv</code>.
     * <p>
     * @param other the value with which to compare.
     * @return <code>eqv(other)</code>
     */
    public boolean equal(Value other)
    { return eqv(other); }

    
    // the java equivalence predicate

    /**
     * Implements Java's <code>equals</code>-equality in terms of
     * Scheme's <code>equal</code>-equality.
     * <p>
     * @param other the object with which to compare.
     * @return <code>(other instanceof Value) && equal((Value)other)</code>
     */    
    public final boolean equals(Object other)
    { return (other instanceof Value) && equal((Value)other); }
    
    
    // scheme io functions
    
    /**
     * Part of Scheme's <code>write</code> function implementation.
     * <p>
     * @param  destination  the java writer which expects the output.
     */
    public abstract void write(Writer destination)
        throws IOException;

    /**
     * Part of Scheme's <code>display</code> function implementation.
     * <p>
     * The default implementation just calls
     * {@link #write write(destination)}.
     * <p>
     * @param  destination  the java writer which expects the output.
     */
    public void display(Writer destination)
        throws IOException
    { write(destination); }


    // the java io function

    /**
     * Implements Java's <code>toString</code> in terms of
     * Scheme's <code>write</code>.
     * <p>
     * @return a java string representation of the value.
     */
    public final String toString()
    {
        StringWriter out = new StringWriter();

        try { write(out); }
        catch (IOException e) { }

        return out.toString();
    }


    // compilation functions

    /**
     * Compiles a value as a literal.
     * <p>
     * @return a newly created {@link Literal}.
     */
    public final Literal getLiteral()
    { return Literal.create(this); }

    /**
     * Compiles a value as normal code.
     * The default implementation assumes the value
     * to be a constant.
     * <p>
     * @param  compilationEnv ignored by this implementation
     * @return {@link #getLiteral()}
     * @see ScmVector#getCode(StaticEnvironment)
     * @see Empty#getCode(StaticEnvironment)
     * @see Pair#getCode(StaticEnvironment)
     * @see Symbol#getCode(StaticEnvironment)
     */
    public Code getCode(StaticEnvironment compilationEnv)
        throws CompileError, TypeError
    { return getLiteral(); }

    /**
     * Compiles a value as list head.
     * A symbol at the head of a list can be a syntactic
     * keyword which needs special treatment.
     * All other values and symbols are compiled
     * as normal code by the default implementation.
     * <p>
     * @return <code>getCode(compilationEnv)</code>
     */
    public Translator getTranslator(StaticEnvironment compilationEnv)
        throws CompileError, TypeError
    { return getCode(compilationEnv); }
}
