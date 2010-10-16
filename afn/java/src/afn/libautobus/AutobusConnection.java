package afn.libautobus;

import org.python.core.PyDictionary;
import org.python.core.PyInteger;
import org.python.core.PyModule;
import org.python.core.PyObject;
import org.python.core.PyString;
import org.python.util.PythonInterpreter;

/**
 * A connection to an Autobus server. You'll typically connect to an Autobus server with
 * this class and go from there.<br/><br/>
 * 
 * This class uses Jython to run the Python version of libautobus under the covers. You
 * don't need to have Python installed; jython.jar contains everything you need. There are
 * a few quirks, however. Probably the most notable ones are that stack traces may look
 * quite a bit different, and you create an AutobusConnection by calling
 * {@link #create(String, int)} instead of by using a constructor. (The reason for this is
 * that libautobus.py subclasses AutobusConnection if it's running under Jython, and
 * create() returns an instance of the libautobus subclass.)
 * 
 * @author Alexander Boyd
 * 
 */
public class AutobusConnection
{
    private static PyDictionary pythonNamespace = new PyDictionary();
    private static PythonInterpreter pythonInterpreter =
            new PythonInterpreter(pythonNamespace);
    private static boolean hasLoadedPython = false;
    private static PyModule libautobus;
    
    private static synchronized void init()
    {
        if (!hasLoadedPython)
        {
            hasLoadedPython = true;
            pythonInterpreter.exec("import sys");
            pythonInterpreter.exec("sys.path += ['../afn-python/src']");
            pythonInterpreter.exec("import libautobus");
            libautobus = (PyModule) pythonNamespace.get("libautobus");
        }
    }
    
    private AutobusConnection()
    {
    }
    
    public static AutobusConnection create(String host, int port)
    {
        init();
        /*
         * This is going to look like an impossible statement, but believe it or not, it
         * actually works.
         */
        return (AutobusConnection) (Object) libautobus.invoke("AutobusConnection",
                new PyString(host), new PyInteger(port));
    }
}
