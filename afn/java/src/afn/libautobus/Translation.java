package afn.libautobus;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

class Translation
{
    @SuppressWarnings("deprecation")
    public static Object encodeObject(Object object)
    {
        if ((object instanceof Boolean) || (object instanceof Integer)
            || (object instanceof Long) || (object instanceof Float)
            || (object instanceof Double) || (object instanceof String))
        {
            return object;
        }
        else if (object instanceof Date)
        {
            Date date = (Date) object;
            return list("timestamp", map("year", date.getYear(), "month", date.getMonth(),
                    "day", date.getDay(), "hour", date.getHours(), "minute", date
                            .getMinutes(), "second", date.getSeconds(), "millisecond", 0));
        }
        else if (object == null)
        {
            return list("null", null);
        }
        else if (object instanceof List)
        {
            ArrayList result = new ArrayList();
            for (Object member : (List) object)
                result.add(encodeObject(member));
            return list("list", result);
        }
        else if (object instanceof Map)
        {
            List keys = new ArrayList();
            List values = new ArrayList();
            for (Entry item : ((Map<?, ?>) object).entrySet())
            {
                keys.add(encodeObject(item.getKey()));
                values.add(encodeObject(item.getValue()));
            }
            return list("map", list(keys, values));
        }
        else if (object instanceof Throwable)
        {
            Throwable t = (Throwable) object;
            return list("exception", map("text", t.getClass().getName() + ": "
                + t.getMessage()));
        }
        throw new IllegalArgumentException("Invalid instance type to encode: "
            + object.getClass().getName());
    }
    
    @SuppressWarnings("deprecation")
    public static Object decodeObject(Object instance)
    {
        /*
         * NOTE: If this message returns an exception type, it MUST be a subclass of
         * RuntimeException (or an instance of RuntimeException itself) for other
         * libautobus code to work properly. For that matter, all libautobus exceptions
         * should inherit from RuntimeException or be instances of RuntimeException
         * itself.
         */
        if ((instance instanceof Boolean) || (instance instanceof Integer)
            || (instance instanceof Long) || (instance instanceof Float)
            || (instance instanceof Double) || (instance instanceof String))
        {
            return instance;
        }
        List instanceAsList = (List) instance;
        String instanceType = (String) instanceAsList.get(0);
        Object value = instanceAsList.get(1);
        if (instanceType.equals("timestamp"))
        {
            Map map = (Map) value;
            return new Date((int) (long) (Long) map.get("year"), (int) (long) (Long) map
                    .get("month"), (int) (long) (Long) map.get("day"),
                    (int) (long) (Long) map.get("hour"), (int) (long) (Long) map
                            .get("minute"), (int) (long) (Long) map.get("second"));
        }
        if (instanceType.equals("null"))
            return null;
        if (instanceType.equals("list"))
        {
            ArrayList result = new ArrayList();
            for (Object member : (List) value)
                result.add(decodeObject(member));
            return result;
        }
        if (instanceType.equals("map"))
        {
            HashMap result = new HashMap();
            List valueAsList = (List) value;
            List keys = (List) valueAsList.get(0);
            List values = (List) valueAsList.get(1);
            for (int i = 0; i < keys.size() && i < values.size(); i++)
            {
                result.put(decodeObject(keys.get(i)), decodeObject(values.get(i)));
            }
            return result;
        }
        if (instanceType.equals("exception"))
        {
            return new RuntimeException((String) ((Map) value).get("text"));
        }
        throw new IllegalArgumentException("Invalid instance type to decode: "
            + instanceType);
    }
    
    /**
     * A utility function that creates a mutable list (an ArrayList, currently) from the
     * values specified as arguments to this function.
     * 
     * @param objects
     *            The objects that should be contained in the returned list
     * @return a new ArrayList containing the specified objects
     */
    public static List list(Object... objects)
    {
        ArrayList list = new ArrayList();
        list.addAll(Arrays.asList(objects));
        return list;
    }
    
    /**
     * A utility function that creates a mutable map (a HashMap, currently) from the keys
     * and values specified as arguments. Arguments are specified as key, value, key,
     * value, etc. As a result, there must be an even number of arguments to this method.
     * 
     * @param objects
     *            The keys and values that should be put into the resulting map
     * @return a new HashMap containing the specified keys and values
     */
    public static Map map(Object... objects)
    {
        if ((objects.length % 2) != 0)
            throw new IllegalArgumentException("Even number of arguments "
                + "must be specified to the map function; "
                + "even-indexed arguments are the keys and "
                + "odd-indexed arguments are the values");
        HashMap map = new HashMap();
        for (int i = 0; i < objects.length; i += 2)
        {
            map.put(objects[i], objects[i + 1]);
        }
        return map;
    }
}
