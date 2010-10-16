package afn.libautobus;

public class NamePair
{
    public final String interfaceName;
    public final String itemName;
    public NamePair(String interfaceName, String itemName)
    {
        super();
        this.interfaceName = interfaceName;
        this.itemName = itemName;
    }
    @Override
    public int hashCode()
    {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((interfaceName == null) ? 0 : interfaceName.hashCode());
        result = prime * result + ((itemName == null) ? 0 : itemName.hashCode());
        return result;
    }
    @Override
    public boolean equals(Object obj)
    {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (!(obj instanceof NamePair))
            return false;
        NamePair other = (NamePair) obj;
        if (interfaceName == null)
        {
            if (other.interfaceName != null)
                return false;
        }
        else if (!interfaceName.equals(other.interfaceName))
            return false;
        if (itemName == null)
        {
            if (other.itemName != null)
                return false;
        }
        else if (!itemName.equals(other.itemName))
            return false;
        return true;
    }
    
}
