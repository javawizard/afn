package jw.jzbot.fact;

/**
 * Represents a data sink that factoid output can be sent to.
 * 
 * @author Alexander Boyd
 * 
 */
public interface Sink
{
    /**
     * Writes a string to this sink.
     * 
     * @param s
     */
    public void write(String s);
    
    /**
     * Writes a single character to this sink.
     * 
     * @param c
     */
    public void write(char c);
    
    /**
     * Writes a base-10 representation of an int to this sink.
     * 
     * @param i
     */
    public void write(int i);
}
