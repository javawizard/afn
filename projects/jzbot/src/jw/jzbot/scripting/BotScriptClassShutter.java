package jw.jzbot.scripting;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;

import org.mozilla.javascript.ClassShutter;

public class BotScriptClassShutter implements ClassShutter
{
    
    @Override
    public boolean visibleToScripts(String fullClassName)
    {
        try
        {
            Class c = Class.forName(fullClassName);
            if (Connection.class.isAssignableFrom(c))
                return true;
            if (PreparedStatement.class.isAssignableFrom(c))
                return true;
            if (ResultSet.class.isAssignableFrom(c))
                return true;
            if (ResultSetMetaData.class.isAssignableFrom(c))
                return true;
            if (DatabaseMetaData.class.isAssignableFrom(c))
                return true;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        /*
         * TODO: should Thread be allowed? This would technically allow scripts
         * to kill the bot by killing all threads, but it's needed to implement
         * setTimeout and setInterval. Consider implementing these in actual
         * java code.
         */
        return fullClassName.equals("java.lang.String")
                || fullClassName.equals("jw.jzbot.scripting.BotScriptObject")
                || (fullClassName.startsWith("java.util.") && fullClassName
                        .split("\\.").length == 2)
                || fullClassName.equals("java.lang.Thread")
                || fullClassName.startsWith("java.util.concurrent.")
                || fullClassName.startsWith("jw.jzbot.com.script.")
                || fullClassName.startsWith("jw.jzbot.utils.script.")
                || fullClassName.equals("org.jibble.pircbot.User");
        
    }
}
