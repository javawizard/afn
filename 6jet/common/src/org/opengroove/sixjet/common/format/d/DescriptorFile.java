package org.opengroove.sixjet.common.format.d;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Scanner;

public class DescriptorFile
{
    public static class DescriptorFileJet
    {
        public int x;
        public int y;
        public int number;
    }
    
    private int width;
    private int height;
    private ArrayList<DescriptorFileJet> jets = new ArrayList<DescriptorFileJet>();
    
    public DescriptorFile(InputStream file) throws IOException
    {
        String fileContents = new Scanner(file).useDelimiter("\\z").next();
        file.close();
        String[] initial = fileContents.split("\n", 2);
        String fileContentsRest = initial[1];
        String header = initial[0];
        Scanner headerScanner = new Scanner(header.trim()).useDelimiter(" ");
        width = headerScanner.nextInt();
        height = headerScanner.nextInt();
        String[] lines = fileContentsRest.split("\n");
        for (String line : lines)
        {
            if (line.trim().equals(""))
                continue;
            Scanner s = new Scanner(line.trim()).useDelimiter(" ");
            s.next();
            DescriptorFileJet jet = new DescriptorFileJet();
            jet.number = s.nextInt();
            jet.x = s.nextInt();
            jet.y = s.nextInt();
            jets.add(jet);
        }
    }
    
    public int getWidth()
    {
        return width;
    }
    
    public int getHeight()
    {
        return height;
    }
    
    public ArrayList<DescriptorFileJet> getJets()
    {
        return jets;
    }
}
