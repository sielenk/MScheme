package MScheme.values;

import MScheme.Value;
import MScheme.List;
import MScheme.Code;

import MScheme.machine.Machine;
import MScheme.environment.StaticEnvironment;
import MScheme.code.CodeList;

import MScheme.exceptions.*;


final class PairOrList
    extends Pair
    implements List
{
    public final static String id
        = "$Id$";


    private PairOrList(boolean isConst, Value first, Value second)
    { super(isConst, first, second); }


    public static List prepend(Value head, List tail)
    { return new PairOrList(false, head, tail); }

    public static Pair create(Value first, Value second)
    { return new PairOrList(false, first, second); }

    public static Pair createConst(Value first, Value second)
    { return new PairOrList(true, first.getConst(), second.getConst()); }


    // implementation of List

    public boolean isEmpty()
    { return false; }

    public boolean isList()
    { return getSecond().isList(); }

    public List toList()
        throws ListExpected
    { return isList() ? this : super.toList(); }

    public int getLength()
        throws ListExpected
    {
        int result = 1;

        for (
            List tail = getTail();
            !tail.isEmpty();
            tail = tail.getTail()
        ) {
            ++result;
        }

        return result;
    }

    public final List getReversed()
        throws ListExpected
    {
        List result = Empty.create();

        for (
            List rest = this;
            !rest.isEmpty();
            rest = rest.getTail()
        ) {
            result = ListFactory.prepend(
                rest.getHead(),
                result
            );
        }

        return result;
    }    


    public Value getHead()
    { return getFirst(); }

    public List getTail()
        throws ListExpected
    { return getSecond().toList(); }


    public Code getCode(StaticEnvironment compilationEnv)
        throws SchemeException
    {
        return
            getHead()
            .getTranslator(compilationEnv)
            .translate(
                compilationEnv,
                getTail()
            );
    }

    public CodeList getCodeList(StaticEnvironment compilationEnv)
        throws SchemeException
    {
        return CodeList.prepend(
            getHead().getCode    (compilationEnv),
            getTail().getCodeList(compilationEnv)
        );
    }
}
