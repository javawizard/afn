package afntest;

import java.util.Map;

import org.json.simple.JSONObject;
import org.json.simple.JSONValue;

public class Json01
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        System.out.println(((Map) JSONValue.parse("{\"value\": 1.0}")).get("value")
                .getClass().getName());
    }
    
}
