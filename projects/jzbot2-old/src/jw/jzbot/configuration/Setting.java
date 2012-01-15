package jw.jzbot.configuration;

import jw.jzbot.configuration.Configuration.VarType;

public abstract class Setting
{
    public final String name;
    public final String description;
    public final VarType type;
    public Folder folder;
    
    protected Setting(String name, String description, VarType type)
    {
        this.name = name;
        this.description = description;
        this.type = type;
    }
    
    @Override
    public int hashCode()
    {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        return result;
    }
    
    @Override
    public boolean equals(Object obj)
    {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Setting other = (Setting) obj;
        if (name == null)
        {
            if (other.name != null)
                return false;
        }
        else if (!name.equals(other.name))
            return false;
        return true;
    }

    public void remove()
    {
        folder.remove(name);
    }
}
