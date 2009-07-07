package jw.tests;

import org.mozilla.javascript.Context;

public class Test02
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        System.out.println(Context.javaToJS(new Character('A'),
                Context.enter().initStandardObjects()).getClass().getName());
    }
    
}
