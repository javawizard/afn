package misc.test;

public class TestRegex
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        String regex = ":\\*buffextras![^ ]+ .* :(jcp|javawizard)![^ ] joined.*";
        String value =
                ":*buffextras!buffextras@znc.in PRIVMSG #jzbot :jcp!~jw@bzflag/contributor/javawizard2539 joined [11:17:28 PM]";
        System.out.println(value.matches(regex));
    }
    
}
