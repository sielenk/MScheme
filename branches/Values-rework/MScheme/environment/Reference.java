package MScheme.environment;

class Reference {
    private int   _level;
    private int   _index;

    public Reference(int level, int index)
    {
        _level = level;
        _index = index;
    }

    public int   getLevel() { return _level; }
    public int   getIndex() { return _index; }

    public String toString()
    {
        return "<" + _level + "-" + _index + ">";
    }

    public boolean equals(Object o)
    {
        if (o instanceof Reference) {
            Reference ref = (Reference)o;

            return (_level == ref._level)
                && (_index == ref._index);
        } else {
            return false;
        }
    }
}
