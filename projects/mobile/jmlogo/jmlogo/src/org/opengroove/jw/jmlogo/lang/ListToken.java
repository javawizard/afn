package org.opengroove.jw.jmlogo.lang;

public class ListToken extends Token
{
    private static int hashCode(Object[] array)
    {
        final int prime = 31;
        if (array == null)
            return 0;
        int result = 1;
        for (int index = 0; index < array.length; index++)
        {
            result = prime * result + (array[index] == null ? 0 : array[index].hashCode());
        }
        return result;
    }
    
    private Token[] members;
    
    public ListToken(Token[] members)
    {
        super();
        this.members = members;
    }
    
    public Token[] getMembers()
    {
        return members;
    }
    
    public int hashCode()
    {
        final int prime = 31;
        int result = 1;
        result = prime * result + ListToken.hashCode(members);
        return result;
    }
    
    public boolean equals(Object obj)
    {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final ListToken other = (ListToken) obj;
        if (members.length != other.members.length)
            return false;
        for (int i = 0; i < members.length; i++)
        {
            if (members[i] == null && other.members[i] != null)
                return false;
            if (members[i] != null && other.members[i] == null)
                return false;
            if (!members[i].equals(other.members[i]))
                return false;
        }
        return true;
    }
}
