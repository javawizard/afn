package jw.jzbot.scripting;

import org.mozilla.javascript.ClassShutter;

public class BotScriptClassShutter implements ClassShutter
{
    
    @Override
    public boolean visibleToScripts(String fullClassName)
    {
        /*
         * TODO: should Thread be allowed? This would technically allow scripts
         * to kill the bot by killing all threads, but it's needed to implement
         * setTimeout and setInterval. Consider implementing these in actual
         * java code.
         */
        return fullClassName.equals("java.lang.String")
                || fullClassName.equals("jw.jzbot.scripting.BotScriptObject")
                || fullClassName.startsWith("java.util.")
                || fullClassName.equals("java.lang.Thread")
                || fullClassName.startsWith("jw.jzbot.com.script.")
                || fullClassName.startsWith("jw.jzbot.utils.script.");
    }
}
