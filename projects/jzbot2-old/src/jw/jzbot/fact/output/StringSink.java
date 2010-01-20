package jw.jzbot.fact.output;

/**
 * A sink that stores its output in a StringBuffer. The current value of the StringBuffer
 * can be obtained at any time.
 * 
 * @author Alexander Boyd
 * 
 */
public class StringSink implements ObservableSink
{
    private StringBuffer buffer = new StringBuffer();
    
    @Override
    public void write(String s)
    {
        buffer.append(s);
    }
    
    @Override
    public void write(char c)
    {
        buffer.append(c);
    }
    
    @Override
    public void write(int i)
    {
        buffer.append(i);
    }
    
    public String toString()
    {
        return getCurrentValue();
    }

    @Override
    public String getCurrentValue()
    {
        return buffer.toString();
    }
}
