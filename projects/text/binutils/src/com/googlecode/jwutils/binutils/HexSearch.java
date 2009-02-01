package com.googlecode.jwutils.binutils;

public class HexSearch
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        String hexcode = args[0];
        if ((hexcode.length() % 2) != 0)
            throw new RuntimeException(
                "hexcode must be a multiple of 2 characters long. Use a leading 0 if it's not.");
        int[] pattern = new int[hexcode.length() / 2];
        for (int i = 0; i < pattern.length; i++)
        {
            String toTranslate = hexcode.substring(i * 2, (i * 2) + 1);
            toTranslate = "0x" + toTranslate;
            pattern[i] = Integer.parseInt(toTranslate, 16);
        }
    }
    
}
