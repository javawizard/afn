package afn.libautobus;

public class InterfaceWrapper
{
    String name;
    AutobusConnection bus;
    
    InterfaceWrapper(AutobusConnection bus, String name)
    {
        this.bus = bus;
        this.name = name;
    }
    
    public FunctionWrapper getFunction(String name)
    {
        return new FunctionWrapper(this, name);
    }
}
