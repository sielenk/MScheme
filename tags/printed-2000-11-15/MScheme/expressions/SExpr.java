package MScheme.expressions;

abstract public class SExpr {

    abstract protected String defaultString();

    final public String toString()
    {
        return defaultString();
    }
}
