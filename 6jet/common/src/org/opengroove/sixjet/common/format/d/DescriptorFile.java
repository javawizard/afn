package org.opengroove.sixjet.common.format.d;

import java.io.InputStream;
import java.util.Scanner;

public class DescriptorFile
{
    public DescriptorFile(InputStream file)
    {
        String fileContents = new Scanner(file).useDelimiter("\\z").next();
    }
}
