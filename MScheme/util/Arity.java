package MScheme.util;


public class Arity
{
    private final int _minArity;
    private final int _maxArity;

    private Arity(int minArity, int maxArity)
    {
        _minArity = minArity;
        _maxArity = maxArity;
    }


    public static Arity exactly(int arity)
    { return new Arity(arity, arity); }

    public static Arity atLeast(int arity)
    { return new Arity(arity, -1); }

    public static Arity inRange(int lo, int hi)
    { return new Arity(lo, hi); }


    public int getMin()
    { return _minArity; }

    public int getMax()
    { return _maxArity; }

    public boolean allowMore()
    { return (_maxArity == -1); }


    public boolean isValid(int arity)
    {
        boolean gotEnoughArguments  = (_minArity <= arity);
        boolean isMaxArityDisabled  = (_maxArity == -1);
        boolean gotTooManyArguments = !isMaxArityDisabled && (_maxArity < arity);

        return (gotEnoughArguments && !gotTooManyArguments);
    }


    public String toString()
    {
        String result = "" + getMin();

        if (allowMore()) {
            result += " or more";
        } else if (getMin() != getMax()) {
            result += " to " + getMax();
        }

        return result;
    }
}
