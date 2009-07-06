package jw.tests;

import jw.jzbot.scripting.BotScriptClassShutter;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.ContextFactory;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.Script;
import org.mozilla.javascript.ScriptableObject;

public class Test01
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        Context context = Context.enter();
        context.setOptimizationLevel(-1);
        // context.setClassShutter(new BotScriptClassShutter());
        context.setLanguageVersion(context.VERSION_1_7);
        context.setMaximumInterpreterStackDepth(1024);
        ScriptableObject scope = context.initStandardObjects();
        Script script = context
                .compileString(
                        "function testfunc(){java.lang.System.out.println('Hello there ' + (3 + 5));}",
                        "testfile.js", 1, null);
        script.exec(context, scope);
        Function function = (Function) scope.get("testfunc", null);
        function.call(context, scope, null, new Object[0]);
    }
    
}
