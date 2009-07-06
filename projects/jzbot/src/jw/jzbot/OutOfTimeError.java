package jw.jzbot;

public class OutOfTimeError extends Error
{
    public OutOfTimeError()
    {
        super("The script used too much time. Each script is "
                + "allowed a maximum of 120 seconds to prevent freezes.");
    }
}
