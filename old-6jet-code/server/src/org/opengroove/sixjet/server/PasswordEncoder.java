package org.opengroove.sixjet.server;

import java.util.Scanner;

import net.sf.opengroove.common.security.Hash;

public class PasswordEncoder
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        System.out.println("Enter a password to encode.");
        System.out.println(Hash.hash(new Scanner(System.in).nextLine()));
    }
    
}
