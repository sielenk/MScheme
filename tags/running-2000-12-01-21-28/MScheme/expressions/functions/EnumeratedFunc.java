package MScheme.expressions.functions;


import MScheme.expressions.SExpr;
import MScheme.expressions.SBool;


abstract public class EnumeratedFunc extends Function
{
    // the operator bootstrap operator
    public final static int OPERATOR = 0;


    // syntax ops
    public final static int DEFINE =  100;
    public final static int SET    =  101;
    public final static int QUOTE  =  102;
    public final static int IF     =  104;
    public final static int LAMBDA =  105;
    public final static int BEGIN  =  106;

    private static EnumeratedFunc _functions[] = {
        SyntaxFunc.DEFINE_FUNC,
        SyntaxFunc.SET_FUNC,
        SyntaxFunc.QUOTE_FUNC,
        SyntaxFunc.IF_FUNC,
        SyntaxFunc.LAMBDA_FUNC,
        SyntaxFunc.BEGIN_FUNC
    };


    private String _name;
    private int    _id;


    protected EnumeratedFunc(
        int    id,
        String name,
        int    minArity,
        int    maxArity
    ) {
        super(minArity, maxArity);
        _name = name;
        _id   = id;
    }

    protected String name() { return _name; }
    protected int    id  () { return _id;   }

    abstract protected boolean evaluateArgs();

    public static SExpr fromId(int id)
    {
        for (int index = 0; index < _functions.length; index++) {
            EnumeratedFunc func = _functions[index];

            if (func.id() == id) {
                return
                    func.evaluateArgs()
                    ? MapFunc.wrap(EvalFunc.INSTANCE, func)
                    : func;
            }
        }
        return SBool.FALSE;
    }


    protected String defaultString()
    {
        return "[" + id() + ": "+ name() + "]";
    }
}
