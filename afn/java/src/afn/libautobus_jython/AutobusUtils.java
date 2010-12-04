package afn.libautobus_jython;

class AutobusUtils
{
    public static Object getDefaultPrimitiveValue(Class type)
    {
        if (type.equals(Void.TYPE))
            return null;
        if (type.equals(Integer.TYPE))
            return new Integer(0);
        if (type.equals(Long.TYPE))
            return new Long(0);
        if (type.equals(Double.TYPE))
            return new Double(0);
        if (type.equals(Boolean.TYPE))
            return Boolean.FALSE;
        if (type.equals(Float.TYPE))
            return new Float(0);
        return null;
    }
}
