package MScheme.values;

import MScheme.Value;

import MScheme.exceptions.ImmutableException;


public interface Pair
    extends Value
{
    String id
        = "$Id$";


    Value getFirst (         );
    void  setFirst (Value fst) throws ImmutableException;

    Value getSecond(         );
    void  setSecond(Value snd) throws ImmutableException;
}
