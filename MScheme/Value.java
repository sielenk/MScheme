package MScheme;

import java.io.Writer;
import java.io.StringWriter;
import java.io.IOException;

import MScheme.values.*;
import MScheme.code.Literal;

import MScheme.environment.Environment;
import MScheme.environment.StaticEnvironment;
import MScheme.exceptions.*;


public interface Value
{
    /** The CVS id of the file containing this class. */
    String id
        = "$Id$";

    /**
     * Returns an immutable version of <code>this</code>.
     */
    Value getConst();

    /**
     * Returns Scheme's boolean interpretation of a value.
     * All values except <code>#f</code> are regarded as
     * true in Scheme.
     */
    boolean isTrue();


    /**
     * Returns <code>true</code> for proper lists.
     * <p>
     * This function is opverridden in the classes
     * {@link Empty} and {@link Pair}.
     */
    boolean isList();

    
    // disjoint types (as in R5RS, 3.2)

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmBoolean}.
     */
    boolean isScmBoolean();

    /**
     * Returns <code>true</code> for instances of 
     * {@link Pair}.
     */
    boolean isPair();

    /**
     * Returns <code>true</code> for instances of 
     * {@link Symbol}.
     */
    boolean isSymbol();

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmNumber}.
     */
    boolean isScmNumber();

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmChar}.
     */
    boolean isScmChar();

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmString}.
     */
    boolean isScmString();

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmVector}.
     */
    boolean isScmVector();

    /**
     * Returns <code>true</code> for instances of 
     * {@link OutputPort} and {@link InputPort}.
     */
    boolean isPort();

    /**
     * Returns <code>true</code> for instances of 
     * {@link Function}.
     */
    boolean isFunction();


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
     */
    List toList() throws ListExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link Pair}.
     */
    Pair toPair() throws PairExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link Symbol}.
     */
    Symbol toSymbol() throws SymbolExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link ScmNumber}.
     */
    ScmNumber toScmNumber() throws NumberExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link ScmChar}.
     */
    ScmChar toScmChar() throws CharExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link ScmString}.
     */
    ScmString toScmString() throws StringExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link ScmVector}.
     */
    ScmVector toScmVector() throws VectorExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link InputPort}.
     */
    InputPort toInputPort() throws InputPortExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link OutputPort}.
     */
    OutputPort toOutputPort() throws OutputPortExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link Function}.
     */
    Function toFunction() throws FunctionExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link Environment}.
     */
    Environment toEnvironment() throws EnvironmentExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link StaticEnvironment}.
     */
    StaticEnvironment toStaticEnvironment() throws EnvironmentExpected;


    // scheme equivalence predicates (as in R5RS, 6.1)

    /**
     * Compares two values for Scheme's <code>eq</code>-equality.
     * This is the most discriminating equality in Scheme and is
     * supposed to be easiest to check.
     * <p>
     * @param other the value with which to compare.
     */
    boolean eq(Value other);
    
    /**
     * Compares two values for Scheme's <code>eqv</code>-equality.
     * This equality is supposed to compare characters and numbers
     * of the same value as equal, even if they are not the same
     * instance.
     * <p>
     * @param other the value with which to compare.
     */
    boolean eqv(Value other);
    
    /**
     * Compares two values for Scheme's <code>equal</code>-equality.
     * This equality compares compound values recursively, primitive
     * values like <code>eqv</code>.
     * <p>
     * @param other the value with which to compare.
     */
    boolean equal(Value other);

    
    // the java equivalence predicate

    /**
     * Implements Java's <code>equals</code>-equality in terms of
     * Scheme's <code>equal</code>-equality.
     * <p>
     * @param other the object with which to compare.
     * @return <code>(other instanceof Value) && equal((Value)other)</code>
     */    
    boolean equals(Object other);


    // scheme io functions
    
    /**
     * Part of Scheme's <code>write</code> function implementation.
     * <p>
     * @param  destination  the java writer which expects the output.
     */
    void write(Writer destination) throws IOException;

    /**
     * Part of Scheme's <code>display</code> function implementation.
     * <p>
     * The default implementation just calls
     * {@link #write write(destination)}.
     * <p>
     * @param  destination  the java writer which expects the output.
     */
    void display(Writer destination) throws IOException;


    // the java io function

    /**
     * Implements Java's <code>toString</code> in terms of
     * Scheme's <code>write</code>.
     * <p>
     * @return a java string representation of the value.
     */
    String toString();


    // compilation functions

    /**
     * Compiles a value as a literal.
     * <p>
     * @return a newly created {@link Literal}.
     */
    Literal getLiteral();

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
    Code getCode(StaticEnvironment compilationEnv)
        throws CompileError, TypeError;

    /**
     * Compiles a value as list head.
     * A symbol at the head of a list can be a syntactic
     * keyword which needs special treatment.
     * All other values and symbols are compiled
     * as normal code by the default implementation.
     * <p>
     * @return <code>getCode(compilationEnv)</code>
     */
    Translator getTranslator(StaticEnvironment compilationEnv)
        throws CompileError, TypeError;
}
